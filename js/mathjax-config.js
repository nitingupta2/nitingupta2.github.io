// Overrides themes/hugo-academic/assets/js/mathjax-config.js.
// Single dollar signs are not math delimiters: the posts use dollar signs for prices (e.g. "$4000 ... $816"),
// and MathJax rendered the text between two prices as math, without spaces. R Markdown posts are not affected,
// because pandoc writes their inline math as \( ... \). Use \( ... \) for inline math in plain Markdown pages,
// and \$ for a literal dollar sign.
window.MathJax = {
  CommonHTML: {linebreaks: {automatic: true}},
  tex2jax: {
    inlineMath: [['\\(', '\\)']],
    displayMath: [['$$', '$$'], ['\\[', '\\]']],
    processEscapes: true
  },
  TeX: {noUndefined: {attributes: {mathcolor: 'red', mathbackground: '#FFEEEE', mathsize: '90%'}}},
  messageStyle: 'none'
};
