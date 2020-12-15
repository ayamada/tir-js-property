# Release

1. `lein test-cljs`
2. Bump up version in `project.clj`
3. `git add . && git commit -S -m 'chore: Releasing X.Y.Z'`
4. `git tag -S -a vX.Y.Z -m '(content of update)'`
5. `lein clean`
6. `lein deploy clojars`
7. Add version to `-ADVANCED` in `project.clj`
    - Or renew version to new `A.B.C-SNAPSHOT` if assured next update
8. `git add . && git commit -S -m 'chore: Released X.Y.Z'`
9. `git push && git push origin --tags`
10. Process to release in github tag pages
