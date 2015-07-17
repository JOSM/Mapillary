## Setting up your local git-repo

```shell
git clone https://github.com/floscher/josm-mapillary-plugin
cd josm-mapillary-plugin
git svn init --prefix=svn/ http://svn.openstreetmap.org/applications/editors/josm/plugins/mapillary #You have to use http://, _not_ https://
git config --local svn.authorsfile authors.txt
git svn fetch #this might take a while
```

## Fetching from the SVN-repo into your local git-repo

```shell
git svn fetch
```

## Building the plugin with Gradle

This project uses the so-called Gradle wrapper. That means you have to install nothing on your machine in order
to build the project. The wrapper consists of the two scripts `gradlew` (for UNIX-based systems like Mac and Linux)
and `gradlew.bat` (for systems running Windows). The following examples shows the commands for Linux/Mac users,
Windows users can simply replace `./gradlew` with `./gradlew.bat`.

For just building the jar-file for the plugin, run
```shell
./gradlew jar
```

If you also want to run the unit tests, create a FindBugs report and a code coverage report, then the following command is for you:
```shell
./gradlew build
```
(look for the results in the directory `build/reports`)

And finally if you have JOSM installed on your machine, you can execute the following to build the plugin from source,
installs it for you in JOSM and then even starts JOSM with the plugin loaded:
```shell
./gradlew runJosm
```

For info about other available tasks you can run
```shell
./gradlew tasks
```

## Making changes to the repo and committing them back to SVN
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

If you want to commit all of the commits that you made on the _‹foo›_-branch back to SVN, then you can skip this step.

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

---

__Pro-tip:__

If you want to use a different text-editor than git currently uses, execute the following command:
`git config --global core.editor ‹insert_your_favourite_text_editor›` and git will in the future always fire the new
editor up instead.

The same applies for the merge-tool: After executing `git config --global merge.tool ‹insert_your_favourite_merge_tool›`
you can omit the --tool option when executing `git mergetool` in the future.
