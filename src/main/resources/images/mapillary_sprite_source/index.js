var fs = require('fs');

var mappingJson = fs.readFileSync('./mapping.json', 'utf8');
var signs = fs.readdirSync('./package_signs');

var mapping = JSON.parse(mappingJson);

signs.forEach(function(signFilename) {
    var signName = signFilename.split('.')[0]
    if (mapping[signName] === undefined) {
        mapping[signName] = {
            'regions': ['eu', 'us'],
        };
    }
});

module.exports = {
    mapping: mapping,
};
