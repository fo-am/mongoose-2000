# Mongoose 2000

A system for remote collection of animal behavior data, for use with the [Banded mongoose research project](https://play.google.com/store/apps/details?id=foam.mongoose) 
in their field site on the Mweya Peninsula, in the Queen Elizabeth National Park, western Uganda.

## Setup
Create a symlink for `eavdb` into  `android\app\src\main\assets`.

On windows you can run `SetupSymlink.bat` to do this

## build
Run `android\gradlew.bat assembledebug` to build or `android\gradlew.bat installDebug` to install onto an attached android device or simulator.