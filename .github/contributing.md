# Contribution

Thank you for contributing to Select. Your contribution helps grow the PureScript ecosystem and make more essential components available to Halogen users.

Following these guidelines helps ensure we're able to carefully consider your contribution and help you finalize your pull request. Your time (like ours) is valuable, and we try to at least provide constructive feedback on every contribution.

### Contributions we love
Select is an open source project and we love to receive contributions from the community. There are many ways to contribute, including:

* Help fix open issues, even if that simply means adding a helpful comment
* Improve the existing example components to better demonstrate Select's features
* Help expand our test coverage
* Update documentation and guides to be easier to understand, more comprehensive, and above all -- up to date!

### Contributions we should discuss
Some contributions will take some discussion before we accept an update to the project. If your contribution includes one of the below examples (or seems to be reasonably similar), please consider reaching out to us before putting in a lot of work to build the feature. We're active on [#fp-chat](https://functionalprogramming.slack.com/) in the PureScript channel (new? [use this link to join](https://fpchat-invite.herokuapp.com/)!) and are reachable via email. For feature requests, feel free to open an issue with a tag.

* New example components (while we accept new examples, we expect a new example to be meaningfully different from existing ones)
* New tutorials (while we love seeing new tutorials, we won't always be able to feature them in the readme)


# Ground Rules

We have a small set of quality-of-life guidelines for contributing to Select. These include:

* All pull requests must pass continuous integration. 
* If you are adding new functionality, you're expected to provide tests and documentation for your code. If you're fixing an existing bug, please provide a failing test case your patch solves. 
* All new example components should be meaningfully different from existing ones and should be added to the ./circleci/run-examples.sh file so they are covered by continuous integration.
* If at all possible, please avoid requiring new dependencies.

### Filing issues
If you have a general question about the project, it is better to ask us on [#fp-chat](https://functionalprogramming.slack.com/) than to open a new issue. If you have run into a bug in the project, then please do open an issue! When you do, we ask that you follow a few steps which are outlined in our issues template. The gist of it is here:

* Verify the problem is indeed with Select (not with Halogen, Pulp, Bower, or PureScript);
* Record what versions you are using for Select, Halogen, and PureScript
* Describe the issue with steps to reproduce (as much as you are able). A minimal reproducible example is the absolute best case scenario.

We promise to address the issue as soon as we can.


### Suggesting features or enhancements
We love to hear about ways we could make Select better. If you're wishing for a feature that doesn't exist in Select, you're probably not alone; there are bound to be others with similar needs. Please feel free to open an issue on GitHub that describes: 

* the feature you would like to see
* why you need it
* how it should work

We promise to review your issue, but we aren't always able to accommodate all requests. It helps if you're able to contribute to the implementation, too!

### Code reviews
The core team looks at pull requests at least weekly, at which point we will review your code, ensure it meets our ground rules and fits with the philosophy of the project, and -- if necessary -- provide constructive feedback. As soon as two members of the team have each signed off on your pull request, we will merge your contribution. We expect responses within two weeks. After two weeks, we may close the pull request if it isn't showing any activity.

# Testing Structure

Select is designed as a group of primitives that can be combined into various types of selection UIs. We test at two levels:

* The primitives themselves, with some standard behaviors
* The primitives used to create several example components

Our continuous integration first attempts to build and test the primitives, and then builds and tests the `/examples/` folder. Tests are built into each sub-project, and Circle CI simply runs them one after another. 

To verify that your implementation is working well, please both build and run tests on the main source folder and run the `run-examples.sh` script in the `.circle-ci` folder.
