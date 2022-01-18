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

const formatActivityData = (activity, records) =>
  records.flatMap(record => {
    const answers = JSON.parse(record[activity] || "[]");
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
    const keys = records.reduce(
      (keys, record) => new Set([...keys, ...Object.keys(record)]),
      new Set()
    );
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

    [
      ["meaning1", "Meaning1"],
      ["meaning2", "Meaning2"],
      ["meaning3", "Meaning3"],
      ["CU1", "CU1"],
      ["CU2", "CU2"],
      ["CU3", "CU3"],
      ["spelling1", "Spelling1"],
      ["spelling2", "Spelling2"],
      ["spelling3", "Spelling3"]
    ].forEach(([elementId, activity]) => {
      displayAsTable(elementId, formatActivityData(activity, data));
    });
  });
