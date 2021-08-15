# jira-issue-history

A very simple tool to trace Jira issue key changes for one ore more given issue IDs.

## Usage


If you don't have one already, generate a Jira API token for your account. See
https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
for details. Save the token in a file -- this tool uses `~/.jira` by default.

Available options and arguments:

```
jira-issue-history - print Jira issues key history

Usage: jira-issue-history [-f|--token-file TOKEN-FILE] (-s|--site ARG)
                          (-u|--user ARG) ISSUE KEYS...
  Fetch Jira issues changelogs and print Key changes history

Available options:
  -f,--token-file TOKEN-FILE
                           File containing JIRA API token. (default: "~/.jira")
  -s,--site ARG            Site name, as in https://<site name>.atlassian.net.
  -u,--user ARG            Your Atlassian username (email).
  -h,--help                Show this help text
```

For example:

```
jira-issue-history --site yourJiraSite --user yourJiraUser FOO-1 FOO-2
FOO-1: BAR-305 -> FOO-1
FOO-2:
```


## Installation

### Using Haskell Stack

Prerequisites: [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

Run `stack install`

### Using Docker

Prerequisites: A working Docker installation


```
docker run --rm -v ~/.jira:/home/stack/.jira petrem/jira-issue-history -s yourJiraSite -u yourJiraUser ISS-1 ISS-2 ...
```

## Contributing

Project: https://github.com/petrem/jira-issue-history

Improvements and bugfixes are welcome. Please create bug reports and enhancement suggestions in GitHub.

## License

Released under BSD license
