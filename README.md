# episim_swinenet
This is the folder for the simulation core of the model. Data import scripts are included by the data is not as it is private data.
The main place to run the simulations is main.R
The main script runs simulations described of the mk_env type which generate the environment with variables, parameters etc...
Each scenario is defined by one km_env file and main.R generates a simulation and report for each mk_env present in the simulation_inputs folder.
The reports are written in the simulation_outputs folder each with a folder and a copy of the original mk_env file for reference.


Note that you need to create an output folder "simulation_outputs" and an input folder "simulation_inputs" in the same folfer as where your "episim_swinenet". Path to data should also be checked for source and processed data although only processed data is needed if no data extractions are needed. The data folder can be found on the shared drive of the VPHI institute.


## Useful git and vi reminders


### Git (share the code)
In order to share your code, you can
- add a file to a commit (`git add <file/folder>` - you can use `git add *` to commit all of your files)
- write your commit message (`git commit -m "<message>"`)
- push your commit to the server (`git push`)

Do not hesitate to `git pull` before pushing new code

If you want to test something but are unshure the code will stay, simply create to an other branch with `git checkout -b <branch name>`. Then, to navigate between branches, use `git checkout <branch name>`. (without the "`-b`")

If you want to see the files you updated, you can use `git status`. The green files are the one you added to you commit, the red ones are untracked.

You can cancel the updates made to a not commited file by using `git checkout <file>`


### This is a good resource on git commands for managing branches:
https://www.nobledesktop.com/learn/git/git-branches

Useful reminders:

To see all local and remote branches, run this command:
git branch -a

See What Branch You're On
git status

Switch to a Branch In Your Local Repo
git checkout my-branch-name

Switch to a Branch That Came From a Remote Repo
To get a list of all branches from the remote, run this command:
git pull
Run this command to switch to the branch:
git checkout --track origin/my-branch-name


Push to a Branch
If your local branch does not exist on the remote, run either of these commands:
git push -u origin my-branch-name
git push -u origin HEAD
NOTE: HEAD is a reference to the top of the current branch, so it's an easy way to push to a branch of the same name on the remote. This saves you from having to type out the exact name of the branch!

If your local branch already exists on the remote, run this command:
git push


### Commands for vi editor:

$ vi <filename>	Open or edit a file.
i	Switch to Insert mode.
Esc	Switch to Command mode.
:w	Save and continue editing.
:wq or ZZ	Save and quit/exit vi.
:q!	Quit vi and do not save changes.
yy	Yank (copy a line of text).
p	Paste a line of yanked text below the current line.
o	Open a new line under the current line.
O	Open a new line above the current line.
A	Append to the end of the line.
a	Append after the cursor's current position.
I	Insert text at the beginning of the current line.
b	Go to the beginning of the word.
e	Go to the end of the word.
x	Delete a single character.
dd	Delete an entire line.
Xdd	Delete X number of lines.
Xyy	Yank X number of lines.
G	Go to the last line in a file.
XG	Go to line X in a file.
gg	Go to the first line in a file.
:num	Display the current line's line number.
h	Move left one character.
j	Move down one line.
k	Move up one line.
l	Move right one character.
