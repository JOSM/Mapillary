If you don't want to tinker with the code, just [install JOSM](https://josm.openstreetmap.de/) and open the Settings dialog in JOSM, choose the Plugin tab, check "Mapillary" and you are ready to go.

But if you want to explore the sourcecode and maybe even improve it, first of all a :thumbsup: for you, and here are the instructions on getting the source code and building it on your machine:

## Setting up your local git-repo

```shell
git clone git@github.com:JOSM/Mapillary.git
cd Mapillary
```

## Building the plugin with Gradle

This project uses the so-called Gradle wrapper. That means you have to install nothing on your machine in order
to build the project. The wrapper consists of the two scripts `gradlew` (for UNIX-based systems like Mac and Linux)
and `gradlew.bat` (for systems running Windows). The following examples shows the commands for Linux/Mac users,
Windows users can simply replace `./gradlew` with `./gradlew.bat`.

If you develop using the Eclipse IDE, run the following command before opening the project in Eclipse. This will download the dependencies and tells Eclipse about where these dependencies are located on your machine:
```shell
./gradlew eclipse
```
As Eclipse plugins we recommend [eclipse-pmd](http://marketplace.eclipse.org/content/eclipse-pmd) and [Anyedit tools](http://marketplace.eclipse.org/content/anyedit-tools).

For just building the jar-file for the plugin, run
```shell
./gradlew jar
```

If you also want to run the unit tests, create a FindBugs report and a code coverage report, then the following command is for you:
```shell
./gradlew build
```
(look for the reports in the directory `build/reports` and for the packaged `Mapillary.jar` in the directory `build/libs`)

And finally, you can execute the following to build the plugin from source, and run the latest JOSM with the Mapillary plugin already loaded.
This works regardless if you have JOSM installed, or which version of it. Any already present JOSM-installation stays untouched by the following command.
```shell
./gradlew runJosm
```

For info about other available tasks you can run
```shell
./gradlew tasks
```
