const https = require("https");

const getJson = url => {
  return new Promise((resolve, reject) => {
    https
      .get(url, resp => {
        let data = "";

        resp.on("data", chunk => {
          data += chunk;
        });

        resp.on("end", () => {
          resolve(JSON.parse(data));
        });
      })
      .on("error", err => {
        reject({ err });
      });
  });
};

module.exports = getJson
