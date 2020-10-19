# tilemancer
Script for GIMP that adds a sprite sheet generation feature to `Filter/Animations/tilemancer`. If you use layer groups to organize different animations of the same sprite, you can generate one row per layer group. Alternatively, use a square shape to optimize file size.

Tilemancer sets the grid size for you when generating, just make sure that the grid is enabled if you want to use that.

## Installation
* Put it in your script folder. you can find the script folder by going to `Edit/Preferences/Folders/Scripts` in GIMP.
* Click on `Filters/Script-Fu/'Refresh Scrips'`

## Example
With the animation frames as layers, its easy to edit and preview using GIMPs `Filter/Animation/Playback` 

[Animated gif](./example.gif)

When the animation looks good, use `tilemancer` to convert it to a sprite sheet
[finished sprite sheet](./example_sheet.png)

## Why
Well, I found some free python scripts floating around the internet for this purpose, but python seems to work poorly/not at all with the newest versions of GIMP. So I took this as an opportunity to learn more about GIMP scripting, and Scheme.

If this GIMP script was helpful for you, consider a small [PayPal donation](https://www.paypal.com/donate/?hosted_button_id=EJ6PDT5HB8Q4C). 
