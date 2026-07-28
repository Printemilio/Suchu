#!/usr/bin/env node
/**
 * Suchu 0.4 -> 0.5 migration: add the now-mandatory statement terminators.
 *
 * In 0.4 a newline ended a statement, so every statement occupied exactly one
 * line unless it was wrapped inside ( ) or [ ] -- which makes a line-based
 * rewrite exact, provided the scanner knows what each bracket means.
 *
 * The two cases that need real tracking:
 *   - `set {1, 2}` is data, not a block, so `xs = set {1, 2}` still needs ';'
 *     while `if x { ... }` does not.
 *   - `case 0 { return "x" }` puts a whole block on one line, so the statement
 *     before the closing brace needs its own ';'.
 *
 *   node tools/migrate-semicolons.mjs [--write] [paths...]
 *
 * Without --write nothing is modified; it only reports what it would do.
 */

import { readFileSync, writeFileSync, readdirSync, statSync } from "node:fs";
import { join, extname } from "node:path";

const args = process.argv.slice(2);
const write = args.includes("--write");
const roots = args.filter((a) => !a.startsWith("--"));

function collect(target, out = []) {
  const info = statSync(target);
  if (info.isDirectory()) {
    for (const entry of readdirSync(target)) collect(join(target, entry), out);
  } else if (extname(target) === ".suchu" || extname(target) === ".md") {
    out.push(target);
  }
  return out;
}

/** Rewrites only the ```suchu fenced blocks of a Markdown file. */
function migrateMarkdown(source) {
  const lines = source.split("\n");
  const out = [];
  let added = 0;
  let i = 0;

  while (i < lines.length) {
    const fence = /^(\s*)```suchu\s*$/.exec(lines[i]);
    if (!fence) { out.push(lines[i]); i += 1; continue; }

    out.push(lines[i]);
    i += 1;
    const body = [];
    while (i < lines.length && !/^\s*```\s*$/.test(lines[i])) { body.push(lines[i]); i += 1; }

    const result = migrate(body.join("\n"));
    out.push(...result.text.split("\n"));
    added += result.added;

    if (i < lines.length) { out.push(lines[i]); i += 1; }
  }

  return { text: out.join("\n"), added };
}

const BLOCK = "block";
const LITERAL = "literal"; // set { ... }

/**
 * One scanner for the whole file, so brace kinds are known even when a block
 * spans many lines. Returns the migrated text and the number of ';' added.
 */
function migrate(source) {
  const lines = source.split("\n");
  const out = [];

  const braces = [];      // stack of BLOCK / LITERAL
  let brackets = 0;       // ( and [ depth, carried across lines
  let added = 0;

  for (const raw of lines) {
    const openBracketsAtStart = brackets;
    let code = "";        // rewritten code portion of this line
    let comment = "";
    let inString = false;
    let lastClosedBrace = null;

    for (let i = 0; i < raw.length; i += 1) {
      const ch = raw[i];

      if (inString) {
        code += ch;
        if (ch === "\\") { code += raw[i + 1] ?? ""; i += 1; }
        else if (ch === '"') inString = false;
        continue;
      }
      if (ch === '"') { inString = true; code += ch; continue; }
      if (ch === "/" && raw[i + 1] === "/") { comment = raw.slice(i); break; }

      if (ch === "(" || ch === "[") { brackets += 1; code += ch; continue; }
      if (ch === ")" || ch === "]") { brackets -= 1; code += ch; continue; }

      if (ch === "{") {
        braces.push(/\bset\s*$/.test(code) ? LITERAL : BLOCK);
        code += ch;
        continue;
      }

      if (ch === "}") {
        const kind = braces.pop() ?? BLOCK;
        lastClosedBrace = kind;
        // A block written entirely on this line still needs its inner ';'.
        if (kind === BLOCK) {
          const before = code.trimEnd();
          const last = before[before.length - 1];
          if (before !== "" && last !== ";" && last !== "{" && last !== "}") {
            code = before + "; ";
            added += 1;
          }
        }
        code += ch;
        continue;
      }

      code += ch;
    }

    const trimmed = code.trimEnd();
    const last = trimmed[trimmed.length - 1];

    const needsTerminator =
      trimmed !== "" &&
      openBracketsAtStart === 0 &&   // not continuing a wrapped literal
      brackets === 0 &&              // and this line closed what it opened
      last !== ";" &&
      last !== "{" &&
      // A '}' ends the statement only when it closed a set literal, not a block.
      (last !== "}" || lastClosedBrace === LITERAL);

    if (needsTerminator) {
      out.push(trimmed + ";" + (comment ? " " + comment : ""));
      added += 1;
    } else if (trimmed === "") {
      // Blank or comment-only: keep the line exactly as written, indentation
      // included -- rebuilding it from the scanner would flush it left.
      out.push(raw.trimEnd());
    } else {
      out.push(trimmed + (comment ? " " + comment : ""));
    }
  }

  return { text: out.join("\n"), added };
}

const targets = roots.length ? roots : ["examples"];
const files = targets.flatMap((t) => collect(t));

let total = 0;
for (const file of files) {
  const source = readFileSync(file, "utf8");
  const { text, added } =
    extname(file) === ".md" ? migrateMarkdown(source) : migrate(source);
  if (added === 0) { console.log(`  ${file.padEnd(44)} unchanged`); continue; }
  if (write) writeFileSync(file, text, "utf8");
  console.log(`  ${file.padEnd(44)} +${added} ';'`);
  total += added;
}

console.log(`\n${write ? "Rewrote" : "Would add"} ${total} terminators across ${files.length} files.`);
if (!write) console.log("Re-run with --write to apply.");
