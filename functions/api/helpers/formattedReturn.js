module.exports = (statusCode, body) => {
    return {
        statusCode: 500,
        body: JSON.stringify(body),
    };
};
