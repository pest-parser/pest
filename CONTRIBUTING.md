# Contributing to pest

## How to contribute

The simplest way to contribute is by opening issues. If you see a mistake in the docs, a bug, or even some bad
indentation in the code, don't hesitate to open up an issue. If you have a feature request or know how to
redesign a key component of pest, an issue is the way to go.

However, if you have the time and energy to make your very own contribution to the project, we'd warmly
encourage you to open a pull request. A maintainer will soon review your code and you can follow up from there.

### Finding something to contribute

The simplest way to get started is to take a look at the issues. If something is not specific enough or not
detailed enough, feel free to leave a comment.

Reach out on [Gitter](https://gitter.im/pest-parser/pest) for more ideas.

## Security Issues

**DO NOT** publically post about the security issue. Doing so will leave others vulnerable until it is patched.

1. Reach out on [Gitter](https://gitter.im/pest-parser/pest) and someone will direct you about what to do. 
   DO NOT report the issue itself on the public channel. Just let them know that you have found something
   and they will direct you about how to disclose the problem appropriately.
2. If that does not work, [create a new issue](https://github.com/pest-parser/pest/issues/new) and mention
   that you have a security issue. We will give you a way to privately contact us about the problem.

## GitHub Issue Labels

The issue labels are broken up into several categories. When you want to add labels to an issue,
start by adding a single label from the `Priority`, `Status`, and `Type` categories. This is usually
all you will have to do for most issues. You will usually only need one of each, but feel free to
use more as you see appropriate.

The `Difficulty` category is optional but highly recommended. It's optional because sometimes
it can be hard to classify an issue right away. Add this label as soon as possible because it
helps new contributors determine what to start working on.

* **Priority:** The importance of the issue. Most issues should be Medium or Low. Anything higher
  usually signals that someone should be working on it right away.
  * `Priority: Low`
    * Probably will not be fixed soon by the core contributors
    * We are open to pull requests from people who need this sooner than we can add it
  * `Priority: Medium`
    * Most issues will start as this
  * `Priority: High`
    * Really important, fix this very soon
    * Someone should be assigned and working on it
  * `Priority: Critical`
    * Fix this immediately
    * Someone should be assigned and working on it
  * `Priority: Needs Triage`
    * This priority label can be used for issues that need to be triaged to decide their priority
* **Status:** The current state of the issue. For some statuses, you may decide to not include a priority.
  * `Status: Need Info`
    * The issue is blocked waiting on information from the user who reported the bug
  * `Status: Blocked`
    * The issue is blocked waiting on another issue or PR
  * `Status: On Hold`
    * The issue is on hold pending something else
  * `Status: Available (not started)`
    * Anyone can pick this issue up as it has not been started (just leave a comment on the issue)
  * `Status: Pending`
    * The issue is next up to be worked on
  * `Status: In Progress`
    * Someone is assigned and the issue is being worked on
    * If the person working on the issue stops, the issue goes back to being available
  * `Status: Completed`
    * The issue has been completed and should be closed
  * `Status: Review Needed`
    * The changes made to resolve the issue or PR need to be reviewed before further progress can be made
  * `Status: Revision Needed`
    * Some revisions are required before further progress can be made
  * `Status: Abandoned`
    * Work on this issue has been abandoned
  * `Status: Won't Fix`
    * Issue does not need to be fixed or will not be fixed for one reason or another
* **Type:** The type of issue. The majority of issues should fit into one of these types.
  * `Type: Bug`
  * `Type: Feature/Enhancement`
  * `Type: Documentation`
  * `Type: Maintenance`
  * `Type: Performance`
  * `Type: Question`
  * `Type: Security`
