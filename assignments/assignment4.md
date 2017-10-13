# Assignment 4

On the development-practices side, Assignment 4 introduces unit testing. On the Haskell side, you will work with function values, and get used to thinking of these functions as your primitive values.

- You will work with functions as your primitive values, and combine them in suitable ways.
- You will continue practicing with version control.
- You will continue managing your project by creating issues and milestones in the GitLab interface.
- You will start writing unit tests for your code.

At this point you should have the following setup:

1. You created a fork of my `functional-programming-assignments` repository in GitLab. It probably has a web address that looks like `https://gitlab.com/skiadas/functional-programming-assignments` but with your name instead of mine.
2. You have also cloned your fork locally on your computers, and it resides in some folder probably called `functional-programming-assignments`.
3. You have added my repository as a new remote called `upstream`. You now need to update your local repository with my "upstream" changes.
    - Open your project in GitKraken and you should be seeing two remotes on the side, origin and upstream. In this step you will update the remote repositories to fetch the upstream update, and update your local upstream with the changes that happened on my repository. You do this with the little arrow to the right of the "Pull" menu, and you select the "Fetch All" option. You should also be seeing in the main window multiple paths. There is your local master, with a little laptop image to it, and if you have pushed recently then you would also see your fork "origin/master", with your gitlab avatar, at the same position. And you will probably see another branch, the "upstream/master", with my picture on it.
    - Make sure the current branch is your local master, it should have a checkbox next to it. Also make sure you have no uncommitted changes (they are usually in a WIP item at the top). Then right-click my branch's latests commit, and you should see a context menu. One of the options would say: "Merge upstream/master into master". Click it.
    - You are ready to go! There should now be a folder containing the assignment4 descriptions. The README in that folder should tell you what you need to do to finish the assignment.

## Project Management and Testing

You should continue using Issues, Milestones, Labels, and Boards to manage the progress on your project. Everything you work on should be part of an issue you have created before you start work, and that issue should have a suitable label and it should be a part of a milestone.

You will need to have unit tests for all functions described in the assignment. Make sure to read the [Testing Notes](notes/testing.md) before working on the assignment. You are expected to write QuickCheck properties as a means of testing your functions.
