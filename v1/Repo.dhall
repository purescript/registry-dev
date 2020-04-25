{-

Some kind of Git thing. We have special support for GitHub because it's easier
for us if packages are there (as we use their API to do things).
So other git hosting providers are supported but the quality of service might
not be on par sometimes.

-}
< GitHub : { owner : Text, repo : Text, version : Text }
| Git : { url : Text, version : Text }
>