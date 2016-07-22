# How do I add new elements?

## SVG requirements
To add new elements to sprites it is important that `.svg` files which are about to get imported, meet below criteria.

* The file cannot contain any bitmap data, it won't be parsed by `spritezero-cli`
* The file has to fit within 24/24px bounds, since that's the size of traffic signs on the map we display. `spritezero-cli` will generate a sprite with whatever size it is given.

## Conversion process
Unfortunately there is no simple way to convert from other formats (e.g. `.eps`). The workflow that is good enough is outlined in _Instructions_. Below is high level overview of what needs to be done. The process is **VERY SIMPLE**, but there aren't any convenient tools for it. If you found/used better tools, I owe you a fancy coffee next time I see you :wink:.

1. Convert from other vector format to `.svg`
2. Resize the `*.svg` to 24x24px size
3. Run sprite generation

## Instructions
Here's an step by step process of how to generate a sprite from two `.eps` files.

1. Prepare your `.eps` files, the original size doesn't matter.
2. Go to [CloudConvert](https://cloudconvert.com/) and drop you `.eps` files. Select conversion to `.svg`.
3. Click **Start Conversion**.
4. After conversion is done, select **Create archive**, select zip. Click **Start Conversion**.
5. After zipping is done, download and unzip the file.
6. Go to [IcoMoon](https://icomoon.io/app).
7. Remove the initial set of icons they provide.
8. Add more steps with screenshots

