/* 
 * WARN: Run this before checking in new svgs to the repository!
 * This scripts adds bounds and contains an svg within a square
 */

const fs       = require('fs');
const xml2js   = require('xml2js');

const file = 'package_signs/' + process.argv[2].replace(/\u001b\[.*?m/g, '')

const applyChanges = (result, out, filename) => {
    result.svg.$.width = out.width;
    result.svg.$.height = out.height;
    result.svg.$.viewBox = out.viewBox;

    result.svg.rect = {
        $: {
            width: '100%',
            height: '100%',
        }
    }
    
    if (result.svg.g && result.svg.g[0]) {
        console.log(' [INFO]: g', filename)
        result.svg.g[0].$.transform = out.transform;
    } else if (result.svg.path[0].$) {
        console.log(' [INFO]: paths', filename)
        for (let path of result.svg.path) {
            path['$']['transform'] = out.transform;
        }
    } else {
        console.log(' [INFO]: else', filename)
    }

    return result;
}

const effs = function (filename) {
    const file = fs.readFileSync(filename, 'utf8');

    xml2js.parseString(file, function (err, result) {
        let w, h, g;

        try {
            w = Number(result.svg.$.width);
            h = Number(result.svg.$.height);
            viewBox = result.svg.$.viewBox.split(' ');

            if (w == h) {
                process.exit(0)
            }

            let width, height, translate;


            if (w > h) {
                width = w;
                height = w;
                translate = [0, (w - h) / 2].join(' ')
            } else {
                width = h;
                height = h;
                translate = [(h - w) / 2, 0].join(' ')
            }

            viewBox[2] = width;
            viewBox[3] = height;

            // Final values
            const out = {
                w, h, width, height,
                viewBox: viewBox.join(' '),
                transform: `translate(${translate})`
            }

            const changes = applyChanges(result, out, filename);
            const builder = new xml2js.Builder();
            const xml = builder.buildObject(changes);

            const outName = filename.split('/')
            fs.writeFileSync('./package_signs/' + outName[outName.length - 1], xml, 'utf8', function (err, data) {
            })

        } catch (e) {
            console.log(' [ERR] ', e);
            process.exit(0)
        }
    });
    process.exit(0)
}

effs(file)
