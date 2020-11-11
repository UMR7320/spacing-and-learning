const Airtable = require("airtable");
require('dotenv').config()

Airtable.configure({
  endpointUrl: process.env.API_URL,
  apiKey: process.env.API_KEY,
});

const base = Airtable.base("appvKOc8FH0j48Hw1");
module.exports = { base };
