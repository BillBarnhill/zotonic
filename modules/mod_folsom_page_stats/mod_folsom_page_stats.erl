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
-mod_prio(9999).
-mod_depends([]).
-mod_provides([folsom_page_stats]).

-compile([{parse_transform, lager_transform}]).

-include_lib("zotonic.hrl").

%% interface functions
-export([
    observe_dispatch/2
]).

%% @doc Log page request. For now it just dumps to console like tracer
observe_dispatch(#dispatch{path=Path}, Context) ->
    ReqData = m_req:m_to_list(#m{value=undefined},Context),
    log_req_data(ReqData, Context),
    undefined.

%% @doc Default callback function for tracefun/2
log_req_data(ReqData, Context) ->
    ?zWarning("--Start Page Request Data --~n~p~n--End Page Request Data--~n~n",
              [ReqData], Context),
    ok.
