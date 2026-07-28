#!/usr/bin/env node
/**
 * Extracts every ```suchu block from the documentation into standalone files so
 * they can be fed to the real parser. Documentation that is never executed
 * drifts from the language -- this is what stops that happening.
 *
 *   node tools/extract-snippets.mjs <out-dir> <content-dir>
 *
 * The documentation lives in its own repository, so pass its content directory
 * explicitly, then run `suchu comp` over every file the tool writes.
 *
 * A block tagged ```suchu ignore is skipped: use it for deliberately broken
 * code shown as a counter-example.
 */

import { readFileSync, readdirSync, mkdirSync, writeFileSync, rmSync } from "node:fs";
import { join, basename } from "node:path";

const outDir = process.argv[2];
const contentDir = process.argv[3];

if (!outDir || !contentDir) {
  console.error("usage: node tools/extract-snippets.mjs <out-dir> <content-dir>");
  console.error("  e.g. node tools/extract-snippets.mjs /tmp/snippets ../suchu-website/docs/content");
  process.exit(1);
}

rmSync(outDir, { recursive: true, force: true });
mkdirSync(outDir, { recursive: true });

let total = 0;
let skipped = 0;

for (const file of readdirSync(contentDir).filter((f) => f.endsWith(".md")).sort()) {
  const lines = readFileSync(join(contentDir, file), "utf8").split("\n");
  const stem = basename(file, ".md");
  let index = 0;
  let i = 0;

  while (i < lines.length) {
    const fence = /^\s*```suchu(\s+ignore)?\s*$/.exec(lines[i]);
    if (!fence) { i += 1; continue; }

    const ignored = Boolean(fence[1]);
    const startLine = i + 1;
    i += 1;

    const body = [];
    while (i < lines.length && !/^\s*```\s*$/.test(lines[i])) { body.push(lines[i]); i += 1; }
    i += 1;

    index += 1;
    if (ignored) { skipped += 1; continue; }

    // The name carries the source location so a failure points straight at it.
    const name = `${stem}--block${String(index).padStart(2, "0")}--line${startLine}.suchu`;
    writeFileSync(join(outDir, name), body.join("\n") + "\n", "utf8");
    total += 1;
  }
}

console.log(`extracted ${total} snippets (${skipped} marked ignore) into ${outDir}`);
