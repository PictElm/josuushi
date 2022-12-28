## [助数詞](https://jisho.org/word/%E5%8A%A9%E6%95%B0%E8%A9%9E)「じょすうし」

Build with:
```console
$ scripts/build && scripts/index
$ scripts/minify # optionally
```

The result is a static site:
- `index.html`
- `style.css`
- `main.js`
- `data/`

Example:
```console
$ serve &
```

## On `data/`

The JSON files under data/ works as the database. Each is names after
the hexadecimal representation of the codepoints of the character used
in the counter itself, preceeded with a `_`.
For example ["日" is `data/_065e5.json`](data/_065e5.json).

The object itself is of the following `Counter` type:
```typescript
type Counter = {
  repr: Ruby,
  tags: string[],
  cases: Record<number, Ruby>,
};
type Ruby = [kanji: string, reading: string];
```

`tags` is a list of \*things\* (usually plural nouns) that the counter applies
for, in English. The name `Ruby` comes from the `<ruby>` HTML element, where
`<rt>` would be `reading`.

The `cases` property lists the special cases in reading/writing for given
numbers. A negative number indicates an exact match, otherwise the case
will be re-used for larger numbers. For example `-3` will be used for `3日`
only, while `3` would be used for `3日`, `13日`, `23日`...

The (generated) `data/index.js` file defines a `counterIndexJSON` variables. It
contains a JSON, which is a list of `[name: string, tags: string[]]`. `name`
is the name of a file in `data/` without its extension (so `"_065e5"` for
the example of `日`).

## Some Resources Used

- [Tae Kim - Numbers and Counting](http://guidetojapanese.org/learn/grammar/numbers)
- [American / Japanese Higher Numbers](https://www.trussel.com/jnumbers.htm)
- [Tofugu - 350 Japanese Counters](https://www.tofugu.com/japanese/japanese-counters-list/)
