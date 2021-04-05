const Airtable = require("airtable");

Airtable.configure({
  endpointUrl: process.env.API_URL,
  apiKey: process.env.API_KEY,
});

const formattedReturn = require("./formattedReturn");
module.exports = async (event) => {
  const fields = event.body;
  try {
    const base = Airtable.base(event.queryStringParameters.app);
    const createdRecord = await base(event.queryStringParameters.base).create([


    ]);
    return formattedReturn(200, createdRecord);
  } catch (err) {
    console.error(err);
    return formattedReturn(500, { "err": err });
  }
};