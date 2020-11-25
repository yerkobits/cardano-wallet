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

# Fetch github comments with the "Test failure" label, and create a map from
# issue number to title and url
#
# Returns e.g.
# {2083=>[{"number"=>2083, "url"=>"https://github.com/input-output-hk/cardano-wallet/issues/2083", "title"=>"Windows integration" }
def fetch_gh_ticket_titlemap
  query = <<~END
    query {
      repository(name: "cardano-wallet", owner: "input-output-hk") {
        issues(labels: ["Test failure"], last: 100) { edges { node {
          number,
          url,
          title
        }}}
      }
    }
  END
  res = sendGithubGraphQLQuery(query)['data']['repository']['issues']['edges']
    .map { |x| x['node'] }
    .group_by { |x| x['number']}

end

puts fetch_gh_ticket_titlemap

#objs = fetch_comments
#for obj in objs do
#   puts ""
#   puts obj
#   puts ""
#end
