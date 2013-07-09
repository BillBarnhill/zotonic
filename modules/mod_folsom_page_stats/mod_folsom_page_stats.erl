%% @author Bill Barnhill <bill.barnhill@communitivity.com>
%% @copyright 2013-2016 Bill Barnhill
%% Date: 2013-06-07
%% @doc A module to maintain page request statistics in Folsom

%% Copyright 2013-2016 Bill Barnhill
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_folsom_page_stats).
-author("Bill Barnhill <bill.barnhill@comunitivity.com>").

-mod_title("Zotonic Page Stats in Folsom").
-mod_description("A module to maintain page request statistics in Folsom").
-mod_prio(500).
-mod_depends([]).
-mod_provides([folsom_page_stats]).

-include_lib("zotonic.hrl").

%% interface functions
-export([
    observe_page_req/2,
    get_page_stats/1,
    get_page_stat_values/1
]).

%% @doc Log page request. For now it just dumps to console like tracer
observe_page_req(Req=#page_req{}, _) ->
    lager:log(info, self(), io_lib:format("(module at info) Dumping request data...~n~p", [Req])),

    Site = Req#page_req.site,
    TS = Req#page_req.timestamp,

    IncTemporal = make_temporal_counter_inc_fn(Site, TS),
    IncTemporal(req, count),
    IncTemporal(req_by_path, Req#page_req.path),
    IncTemporal(req_by_referer, Req#page_req.referer),
    IncTemporal(req_by_agent, Req#page_req.agent),
    undefined.

%% @doc Create a function that increments a counter
%%      , creating the counter if it didn't already
%%      exist.
make_temporal_counter_inc_fn(Site, Timestamp) ->
    Date = erlang:element(1, Timestamp),
    Time = erlang:element(2, Timestamp),
    TimePerHour = erlang:setelement(3, erlang:setelement(2, Time, all), all),
    AllTime = {all, all, all},
    DatePerMonth = erlang:setelement(3, Date, all),
    fun (Category, Id) ->
    	Name = {Site, Category, Id},
       	inc_counter(Name),
	inc_counter({Name, {Date, TimePerHour}}),
	inc_counter({Name, {Date, AllTime}}),
	inc_counter({Name, {DatePerMonth, AllTime}})
    end.

%% @doc Increment a Folsom counter, creating it if
%%      it didn't already exist.
inc_counter(Name) ->
    folsom_metrics:notify(Name, {inc, 1}, counter).

%% @doc Return the metric names for page statistics
get_page_stats(Site) ->
     [Name || Name = {{SiteActual, _, _}, _} <- folsom_metrics:get_metrics(), SiteActual =:= Site].

%% @doc Return the metric names and their values,
%%      as a proplist
get_page_stat_values(Site) ->
      [ {Name, folsom_metrics:get_metric_value(Name)} || Name <- get_page_stats(Site)].