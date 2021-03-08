const plugin = require("tailwindcss/plugin");

module.exports = {
  purge: [
    "pf_app/**/*.purs",           // Inline classes
    "pf_app/css/tailwind.css",  // Composed classes
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
      textColor: ['visited']
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
      }

      addUtilities(scrollSnap, {
        variants: ["responsive"]
      })
    })
  ],
}
