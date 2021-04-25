const plugin = require("tailwindcss/plugin");

module.exports = {
  mode: 'jit',
  purge: [
    "src/**/*.purs",     // Inline classes
    "css/tailwind.css",  // Composed classes
  ],
  darkMode: false, // or 'media' or 'class'
  theme: {
    fontFamily: {
      'sans': ['Montserrat']
    },
    extend: {
      colors: {
        "faint": {
          DEFAULT: "#fffdf2",
          100: "#f2f0e6",
          200: "#e5e2ce",
        },
      },
      backgroundImage: theme => ({
        'pixel-pattern': "url('https://avatars.githubusercontent.com/u/66708316?v=4')"
      }),
    },
  },
  variants: {
    extend: {
      textColor: ['visited'],
      translate: ['focus'],
    },
  },
  plugins: [
    plugin(function({ addUtilities }) {
      const scrollSnap = {
        ".scroll-snap-y-proximity": {
          "scroll-snap-type": "y proximity",
        },
        ".scroll-snap-align-start": {
          "scroll-snap-align": "start",
        },
        ".no-scroll-snap-type": {
          "scroll-snap-type": "none",
        },
        ".no-scroll-snap-align": {
          "scroll-snap-align": "none",
        },
      };

      addUtilities(scrollSnap, {
        variants: ["responsive"]
      });
    }),
    plugin(function({ addUtilities }) {
      const hoverBox = {
        ".hover-box:hover::after": {
          "content": '""',
          "display": "block",
          "position": "absolute",
          "width": "100%",
          "height": "15px",
          "bottom": "-15px",
        },
      };

      addUtilities(hoverBox, { });
    }),
    plugin(function({ addUtilities }) {
      const textUnderline = {
        ".text-underline": {
          "position": "relative",
        },
        ".text-underline::after": {
          "content": '""',
          "display": "block",
          "position": "absolute",
          "background-color": "black",
          "width": "0",
          "height": "5px",
          "left": "50%",
          "transition": "width 0.3s ease 0s, left 0.3s ease 0s",
        },
        ".text-underline:hover::after": {
          "width": "100%",
          "left": "0",
        },
      };

      addUtilities(textUnderline, { });
    }),
  ],
};
