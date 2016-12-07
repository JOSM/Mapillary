var fs = require('fs');

var mappingJson = fs.readFileSync('./mapping.json', 'utf8');

module.exports = {
    mapping: JSON.parse(mappingJson),
};
