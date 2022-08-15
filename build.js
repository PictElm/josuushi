#!/usr/bin/env node
/**
 * build the app (`index.html`):
 * - embed JSON from data/
 * - run `elm make`
 */
const fs = require('fs');
const ps = require('child_process');

console.log("Reading src/Embedded.elm");
const source = fs.readFileSync("src/Embedded.elm").toString();
console.log("Saving a copy of src/Embedded.elm");
fs.renameSync("src/Embedded.elm", "Embedded.elm.~");
console.log("Locating #region to edit");
const start = source.indexOf("\n", source.indexOf("#region")) + 1;
const end = source.lastIndexOf("\n", source.indexOf("#endregion", start));
const slice = source.slice(start, end).replace(/\(/g, "[").replace(/\)/g, "]");
const indent = slice.slice(0, slice.indexOf("["));
console.log("Reading specified files");
const result = `${indent}[ ${JSON.parse(slice)
  .map(pair => {
    try {
      let res = fs.readFileSync(pair[1]).toString();
      if (pair[1].endsWith(".json"))
        try {
          const obj = JSON.parse(res);
          res = JSON.stringify(obj);
          console.log(`- [${pair[0]}]: ${pair[1]} embedded as JSON`);
        } catch { console.log(`- [${pair[0]}]: ${pair[1]} WARN: JSON parsing failed (embedded as text)`); }
      else console.log(`- [${pair[0]}]: ${pair[1]} embedded as text`);
      return [pair[0], res];
    } catch { console.log(`- [${pair[0]}]: ${pair[1]} ERROR: could not open file`); }
  })
  .filter(it => it)
  .map(pair => `("${pair[0]}", """${pair[1]}""")`).join(`\n${indent}, `)}\n${indent}]`;
console.log("Overwriting src/Embedded.elm");
fs.writeFileSync("src/Embedded.elm", source.slice(0, start) + result + source.slice(end));
console.log("Executing `elm make src/Main.elm`");
ps.spawnSync("elm", ["make", "src/Main.elm"], { stdio: 'inherit' });
console.log("Restoring src/Embedded.elm");
fs.renameSync("Embedded.elm.~", "src/Embedded.elm");
