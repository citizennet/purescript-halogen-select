# CSS

This folder contains our modified version of Tailwind.css for the Select library.

- Use `npm run watch` when working on the CSS for automatic builds
- Use `npm run build-prod` when ready to save a new version

## Webpack

This project uses Webpack with PostCSS, PurgeCSS, and ExtractText plugins to generate a CSS file from the Tailwind configuration, remove any unused code based on our JS files, and minify the result.
