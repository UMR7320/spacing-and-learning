const formatVKSdata = records => {
  return records.flatMap(record => {
    return [
      "PreTestAnswers",
      "PosTestAnswers",
      "PostTestDiffAnswers",
      "SurprisePostTestAnswers"
    ].flatMap(session => {
      const answers = JSON.parse(record[session] || "[]");
      answers.forEach(answer => {
        answer.session = session;
        answer.userUID = record.UserUID;
      })
      return answers;
    });
  });
};

const displayVKSdata = records => {
  const keys = Object.keys(records[0]);
  let html = "<table><thead><tr>";
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

  document.getElementById("root").insertAdjacentHTML("afterbegin", html);
}

fetch("/.netlify/functions/api?app=appvKOc8FH0j48Hw1&base=VKS_output&view=all")
  .then(response => response.json())
  .then(json => json.records)
  .then(formatVKSdata)
  .then(displayVKSdata);
