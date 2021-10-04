const fs = require("fs/promises");
const glob = require("glob");
const nunjucks = require("nunjucks");
const path = require("path");

nunjucks.configure(["html"]);

async function main() {
    await fs.mkdir("./public", { recursive: true });
    let pages = glob.sync("html/pages/**/*.html");
    pages.forEach(async page => {
        page = page.split("/").slice(2).join("/");

        let dir = path.parse(page).dir;
        if (dir != "") {
            await fs.mkdir(`./public/${dir}`, { recursive: true });
        }

        let result = nunjucks.render(`./pages/${page}`);
	await fs.writeFile(`./public/${page}`, result);
    });
}

main();
