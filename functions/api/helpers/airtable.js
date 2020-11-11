const Airtable = require("airtable");
require('dotenv').config()

Airtable.configure({
  endpointUrl: process.env.API_URL,
  apiKey: process.env.API_KEY,
});

const base = Airtable.base(process.env.API_CLIENT_ID_RELEARNING);
module.exports = { base };
