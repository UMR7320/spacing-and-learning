const formatData = (prefix, records) =>
  records.flatMap(record =>
    [
      `${prefix}_preTest`,
      `${prefix}_postTest`,
      `${prefix}_postTestDiff`,
      `${prefix}_surprisePostTest`
    ].flatMap(session => {
      const answers = JSON.parse(record[session] || "[]");
      answers.forEach(answer => {
        answer.session = session.replace(`${prefix}_`, "");
        answer.userUID = record.UID;
        answer.condition = record.Group;
        answer.proficiency = record.YesNo;
      });
      return answers;
    })
  );

const formatMeaning1Data = records =>
  records.flatMap(record => {
    const answers = JSON.parse(record["Meaning1"] || "[]");
    answers.forEach(answer => {
      answer.userUID = record.UID;
      answer.condition = record.Group;
      answer.proficiency = record.YesNo;
    });
    return answers;
  });

const displayAsTable = (elementId, records) => {
  let html;
  if (records.length === 0) {
    html = "No data available";
  } else {
    const keys = Object.keys(records[0]);
    html = "<table><thead><tr>";
    for (key of keys) {
      html += `<td>${key}</td>`;
    }
    html += "</tr></thead><tbody>";
    for (record of records) {
      html += "<tr>";
      for (key of keys) {
        html += `<td>${record[key]}</td>`;
      }
      html += "</tr>";
    }
    html += "</tbody><table>";
  }

  document.getElementById(elementId).insertAdjacentHTML("afterbegin", html);
};

const displayData = (elementId, prefix, records) =>
  displayAsTable(elementId, formatData(prefix, records));

fetch(
  "/.netlify/functions/api?app=appvKOc8FH0j48Hw1&base=users&view=VKS_output"
)
  .then(response => response.json())
  .then(json => json.records)
  .then(data => {
    [
      ["vks", "VKS"],
      ["spr", "SPR"],
      ["sentence-completion", "SentenceCompletion"],
      ["acceptability", "Acceptability"]
    ].forEach(([elementId, prefix]) => {
      displayData(elementId, prefix, data);
    });

    displayAsTable("meaning1", formatMeaning1Data(data));
  });
