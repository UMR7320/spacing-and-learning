module.exports = {
  purge: {
    content: ["./src/**/*.elm", "index.js"],
  },
  theme: {
    extend: {},
  },
  variants: { borderColor: ['responsive', 'group-hover', 'group-focus', 'active'] },
  plugins: [],
};
