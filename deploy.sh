# Add another origin
git remote add other-origin https://github.com/jobs-george/notes-probability.git

# Create a new branch from the latest commit
git checkout -b temp-branch HEAD~0

# Push the new branch to the other-origin remote
git push other-origin temp-branch:main
