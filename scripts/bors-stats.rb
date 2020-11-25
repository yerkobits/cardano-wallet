#!/usr/bin/env ruby
#
require 'net/http'
require 'uri'
require 'json'
require 'date'
require 'ansi'

def sendGithubGraphQLQuery(qry)
  githubApiToken = ENV.fetch("GITHUB_API_TOKEN")
  header = { 'Authorization': 'bearer ' + githubApiToken }
  url = "https://api.github.com/graphql"
  uri = URI.parse(url)
  data = {query: qry}
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true
  request = Net::HTTP::Post.new(uri.request_uri, header)
  request.body = data.to_json
  return JSON.parse(http.request(request).body)
end

# A parsed bors comment corresponding to a succeeding or failing build
BorsComment = Struct.new(:url, :bodyText, :createdAt, :succeeded) do
  def to_s
    self.pretty
  end

  def pretty
    color = (succeeded ? ANSI.green : ANSI.red)
    return color + createdAt.strftime("%d %b %H:%M") + ANSI.clear + "\n" + bodyText
  end
end

def fetch_comments
  numberPRsToFetch = 10
  numberCommentsToFetch = 100
  query = <<~END
    query {
    repository(name: "cardano-wallet", owner: "input-output-hk") {
      pullRequests(last: #{numberPRsToFetch}) { edges { node {
        comments(first: #{numberCommentsToFetch}) { edges { node {
          bodyText,
          createdAt,
          url,
          author {
              login
          }
        }}}
      }}}
    }
    }
  END
  response = sendGithubGraphQLQuery(query)
  return response['data']['repository']['pullRequests']['edges']
      .map { |x| x['node']['comments']['edges']}
      .flatten
      .map { |x| x['node']}
      .filter { |x| x['author']['login'] == "iohk-bors" }
      .map do |x|
            BorsComment.new( x['url'] , x['bodyText'], DateTime.parse(x['createdAt']), (x['bodyText'].include? "Build succeeded"))
          end
end

objs = fetch_comments
for obj in objs do
   puts ""
   puts obj
   puts ""
end
