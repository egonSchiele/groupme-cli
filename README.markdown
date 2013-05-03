# groupme-cli

A command line interface to GroupMe.

## Screenshots

![](http://static.adit.io/images/groupme1.png)
![](http://static.adit.io/images/groupme2.png)

## Advantages
- loads 100 messages at the start instead of 20, better for catching up and context
- custom notifications...if a message matches your regex, you will be notified!
- less obtrusive than a website!

## Disadvantages
- no images
- can't like a message

## Usage

    # list all groups
    groupme -t ACCESS_TOKEN -l

    # join a group
    groupme -t ACCESS_TOKEN -u USER_ID -g GROUP_ID

You can get your user_id and group_id by getting information with `-l`.

If you need an access token, head to http://dev.groupme.com.

## Installation
If you're on a mac, [download the binary here](http://static.adit.io/downloads/groupme/groupme-0.0.1-MacOSX.zip).

## Building from source

It's easy! First grab the Haskell platform from here: http://www.haskell.org/platform/
Then:

    cd groupme-cli
    cabal install groupme.cabal

Pull requests welcome...if you want a feature added, send me a pull request!
