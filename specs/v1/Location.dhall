let Subdir = { subdir : Optional Text }

let GitHub = Subdir //\\ { githubOwner : Text, githubRepo : Text }

let Git = Subdir //\\ { gitUrl : Text }

in  < GitHub : GitHub | Git : Git >
