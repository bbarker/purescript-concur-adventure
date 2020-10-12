# Purescript Concur Adventure

A starter kit for making choose-your-own-adventure experiences
with PureScript-Concur. These experiences could be games or simple
stories. The [current demo](https://bbarker.github.io/purescript-concur-adventure/) is relatively simple, though it does use
markup instead of plain text.

Uses Npm, Spago, Google Closure Compiler, and Parcel. Builds tiny 180KB uncompressed bundles!

## Usage

The experiences are optionally path-dependent, as the `nextPage` can depend
on an `Array n` of past events of type `n`. Additionally, they can
depend on data of type `d`, if it exists.

Other than implementing instances of `StoryPage`, the user may want
change the `runPage` function to configure how subsequent pages
are displayed (e.g. how many, what order, etc); currently the
default is to show the complete history, starting with the most
recent page first.

### Grab the code

This is a template repository, so just click the "Use this template"
button above.

### Build code

> npm install

> npm run dev

### Run Dev Server

> npm start

## Hot code reload with PureScript code

At the end of the previous command, you will have a development server
which will watch for changes, and automatically reload the web page.
This mechanism only works with JS changes.

However, in practice, your IDE should automatically recompile PureScript to
Javascript on every change, which will be picked up by the development server.
So you get immediate recompilation even with PureScript.

### Build production artifacts

> npm run prod
