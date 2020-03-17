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

### Instructions
Here's an step by step process of how to generate a sprite from two `.eps` files.

#### 1. Prepare your `.eps` files
The original size doesn't matter. Make sure they have no bitmap data in them.
![00-prepare-eps](https://cloud.githubusercontent.com/assets/1574656/17060961/88e60798-502c-11e6-9ed7-dafa6f09c4dc.png)

#### 2. Go to [CloudConvert](https://cloudconvert.com/) and drop you `.eps` files
Select conversion to `.svg`. Click **Start Conversion**.
![01-to-convert](https://cloud.githubusercontent.com/assets/1574656/17060960/88e524cc-502c-11e6-969f-449c2c3d0232.png)

#### 3. Create a zip archive of your converted files
After conversion is done, select **Create archive**, select zip. Click **Start Conversion**. After zipping is done, download and unzip the file.
![02-create-zip](https://cloud.githubusercontent.com/assets/1574656/17060962/88e797d4-502c-11e6-9181-02abc67a7b6c.png)

![03-download-zip](https://cloud.githubusercontent.com/assets/1574656/17060963/88f2ee40-502c-11e6-9603-e637b82db5e9.png)

#### 4. Go to [IcoMoon](https://icomoon.io/app)
Unfortunately there are no reliable CLI tools that resize `.svg`s.

#### 5. Remove the initial set of icons they provide
![04-remove-spam](https://cloud.githubusercontent.com/assets/1574656/17060964/88f30e98-502c-11e6-96aa-412b5fe221be.png)

#### 6. Upload your `.svg` files
![05-upload](https://cloud.githubusercontent.com/assets/1574656/17060965/88f530ba-502c-11e6-89ac-accb14825118.png)
![06-after-upload](https://cloud.githubusercontent.com/assets/1574656/17060966/88fed08e-502c-11e6-8ea9-d14c560f86f4.png)

#### 7. Select your files in the IcoMoon app
![07-select-all](https://cloud.githubusercontent.com/assets/1574656/17060967/8902fc68-502c-11e6-8438-b8a23117d089.png)
![08-after-selection](https://cloud.githubusercontent.com/assets/1574656/17060968/8909d56a-502c-11e6-89f3-781231fd7d07.png)

#### 8. Go to settings and adjust size to 24px
This step is the only reason we're using IcoMoon app...
![09-settings](https://cloud.githubusercontent.com/assets/1574656/17060969/890d5b5e-502c-11e6-95c5-bab694296e77.png)
![10-set-size](https://cloud.githubusercontent.com/assets/1574656/17060970/890d96f0-502c-11e6-8866-d1db6df03023.png)

#### 9. Download your resized files and profit.
Or, more precisely: check them into this repository and generate your sprites.
![11-profit](https://cloud.githubusercontent.com/assets/1574656/17060971/890ed6c8-502c-11e6-9ceb-160829c28fda.png)

![12-where-are-the-files](https://cloud.githubusercontent.com/assets/1574656/17061279/090ed28c-502e-11e6-8c6f-eb00df452adf.png)
