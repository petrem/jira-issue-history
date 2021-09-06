# jira-issue-history

A very simple tool to trace Jira issue key changes for issue IDs given on the command
line or obtained from a JQL query.


## Usage and configuration

If you don't have one already, generate a Jira API token for your account. See
https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
for details.

Available options and arguments:

```
jira-issue-history - print Jira issues key history

Usage: jira-issue-history [(-c|--config-file CONFIG-FILE) |
                            [-f|--token-file TOKEN-FILE] (-s|--site ARG)
                            (-u|--user ARG)]
                          ((-j|--jql JQL Query) | (-i|--issues)) [ISSUE KEYS...]
                          [--version]
  Fetch Jira issues changelogs and print Key changes history

Available options:
  -c,--config-file CONFIG-FILE
                           YAML config file. (default: "~/.atlassian.yaml")
  -f,--token-file TOKEN-FILE
                           File containing JIRA API token. (default: "~/.jira")
  -s,--site ARG            Site name, as in https://<site name>.atlassian.net.
  -u,--user ARG            Your Atlassian username (email).
  -j,--jql JQL Query       Show changes for issues returned by JQL query.
  -i,--issues              Show changes for individual Jira issues given as
                           arguments.
  --version                Show version.
  -h,--help                Show this help text
```

### Defining/configuring Jira endpoint and authentication

The tool supports reading the token from a file -- `~/.jira` by default, containing the token only.

A YAML configuration file is also supported, with the following content:

    atlassian_username: "<username/email>"
    atlassian_server: "https://<yoursite>.atlassian.net"
    atlassian_api_token: "<API token>"

So there are two ways to run depending how you want to configure.

If you decide to put your token in a `.jira` file:

```
jira-issue-history --site yourJiraSite --user yourJiraUser --issues FOO-1 FOO-2
```

or if you want to use a `.atlassian.yaml` file:

```
jira-issue-history --issues FOO-1 FOO-2
```

### Defining the list of issues to display key history

You can either list individual issues on the command line:

```
jira-issue-history --issues FOO-1 FOO-2 ...
```

or run a JQL query and show the resulting issues histories:

```
jira-issue-history --jql 'project = "FOO"'
```

## Installation

### Using Haskell Stack

Prerequisites: [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

Run `stack install`

### Using Docker

Prerequisites: A working Docker installation


```
docker run --rm -t -v ~/.jira:/home/stack/.jira petrem/jira-issue-history -s yourJiraSite -u yourJiraUser --issues ISS-1 ISS-2 ...
```

or with a `.atlassian.yaml` config file

```
docker run --rm -t -v ~/.atlassian.yaml:/home/stack/.atlassian.yaml petrem/jira-issue-history --issues ISS-1 ISS-2 ...
```

## Contributing

Project: https://github.com/petrem/jira-issue-history

Improvements and bug fixes are welcome. Please create bug reports and enhancement suggestions in GitHub.

## License

Released under BSD license
