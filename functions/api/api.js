// Docs on event and context https://www.netlify.com/docs/functions/#the-handler-method
const Airtable = require("airtable");
const formattedReturn = require("./helpers/formattedReturn");
const createRecord = require("./helpers/createRecord");
const getRecords = require("../helpers/getRecords");

exports.handler = async (event) => {
  if (event.httpMethod === "GET") {
    return await getRecords(event);
  } else if (event.httpMethod === "POST") {
    return await createRecord(event);
  } else {
    return formattedReturn(405, {});
  }
};
