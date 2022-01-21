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

const downloadCSV = event => {
  const table = event.target.parentNode.querySelector("table");
  const data = [];
  const rows = table.querySelectorAll("tr");

  for (var i = 0; i < rows.length; i++) {
    const row = [];
    const cols = rows[i].querySelectorAll("td, th");

    for (var j = 0; j < cols.length; j++) {
      row.push(cols[j].innerText);
    }

    data.push(row.join(";"));
  }

  const csvFile = new Blob([data.join("\n")], { type: "text/csv" });
  const downloadLink = document.createElement("a");
  downloadLink.download = `${table.parentNode.id}-${new Date().toUTCString()}.csv`;
  downloadLink.href = window.URL.createObjectURL(csvFile);
  downloadLink.style.display = "none";
  document.body.appendChild(downloadLink);
  downloadLink.click();
};

const displayAsTable = (elementId, records) => {
  let html;
  if (records.length === 0) {
    html = "<div>No data available</div>";
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
  const button = document.createElement("button");
  button.innerHTML = "Download";
  button.addEventListener("click", downloadCSV);
  document.getElementById(elementId).prepend(button);
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
