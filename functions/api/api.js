// Docs on event and context https://www.netlify.com/docs/functions/#the-handler-method
const Airtable = require("airtable");
const formattedReturn = require("./helpers/formattedReturn");
const createRecord = require("./helpers/createRecord");
const getRecords = require("./helpers/getRecords");
const updateRecord = require("./helpers/updateRecord");

exports.handler = async (event) => {
  if (event.httpMethod === "GET") {
    return await getRecords(event);
  } else if (event.httpMethod === "POST") {
    return await createRecord(event);
  } else if (event.httpMethod === "PATCH") {
    return await updateRecord(event);
  } else {
    return formattedReturn(405, {});
  }
};
