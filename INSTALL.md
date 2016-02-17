If you don't want to tinker with the code, just [install JOSM](https://josm.openstreetmap.de/) and open the Settings dialog in JOSM, choose the Plugin tab, check "Mapillary" and you are ready to go.

But if you want to explore the sourcecode and maybe even improve it, first of all a :thumbsup: for you, and here are the instructions on getting the source code and building it on your machine:

## Setting up your local git-repo

```shell
git clone git@github.com:floscher/josm-mapillary-plugin.git
cd josm-mapillary-plugin
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

---

If you don't have push-access to the SVN-server, you should now be ready to go.

The following paragraphs only deal with transferring commits from the git-repository to the SVN-server and the other way around.

---

## Connecting the git-repo to the SVN-server (optional)

This step is normally only relevant, if you either have push-access to the SVN-server and want to push your commits from the git-repo to the SVN-repo. Otherwise just skip it.

First, you need to have [`git-svn`](https://git-scm.com/docs/git-svn) installed. E.g. on Ubuntu, just run `sudo apt install git-svn`. On Windows you probably already installed it together with `git`.

Then run the following commands:
```shell
git svn init --prefix=svn/ http://svn.openstreetmap.org/applications/editors/josm/plugins/mapillary #You have to use http://, _not_ https://
git config --local svn.authorsfile authors.txt
mkdir .git/refs/remotes/svn
git rev-parse master > .git/refs/remotes/svn/git-svn # creates a file containing the SHA1 of master-branch
git svn fetch
git reset --hard svn/git-svn
```

## Making changes to the repo and committing back to SVN (if you have git-svn set up as described above)

The following steps are for those with commit-privileges for the SVN repository containing the plugins for JOSM.
All others can simply file pull requests against the master-branch on github.

We recommend, that you start your development at the head of the master-branch in a separate branch (in this example
it's called _‹foo›_, you can name it what you like, but best call it after the feature you are working on):
```shell
git checkout origin/master
git branch ‹foo›
```

---

Then commit your changes to this branch _‹foo›_ until you feel it's time for committing them back to SVN:
```shell
git commit
```

---

If you want to push (or in SVN-terms _commit_) all of the commits that you made on the _‹foo›_-branch back to SVN, then you can skip this step.

Otherwise execute the following line to preserve the other commits:
```shell
git branch tmp
```
This creates a new branch called _tmp_ which saves those commits for later, which are not rebased.

---

Then fetch the current state of the SVN-repository to avoid merge conflicts:
```shell
git svn fetch
```

---

Now you should rebase onto the current state of the SVN-repository:
```shell
git rebase --interactive svn/git-svn
```
A text editor should open with all commits on the _‹foo›_-branch that are currently not in SVN. Delete all lines except
the ones containing those commits you want to commit to SVN.

Watch the command line. If it says, that merge conflicts have occured you'll first have to resolve these conflicts.
For example with the following command:
```shell
git mergetool --tool=‹name_of_your_mergetool›
```
Possible mergetools include emerge, gvimdiff, kdiff3, meld, vimdiff and tortoisemerge.

After merging you'll have to tell git that it should complete the rebasing:
```shell
git rebase --continue
```

If it still says that there are merge conflicts, go back to the `git mergetool`-command and repeat the steps from there on.

---

You have reached the final step, the following command will now interact with the SVN-server to commit your changes
to the SVN-repository:
```shell
git svn dcommit --interactive --username=‹your_svn_username›
```
This command will ask for your password and shows you the commit message of every git-commit before it
applies it to the SVN-repo.
