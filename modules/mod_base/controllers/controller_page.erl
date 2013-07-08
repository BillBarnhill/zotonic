%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Basic page

%% Copyright 2009 Marc Worrell
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

-module(controller_page).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/2,
    previously_existed/2,
    is_authorized/2,
    html/1,
    get_id/1
]).

-include_lib("controller_html_helper.hrl").

%% @doc Check if the id in the request (or dispatch conf) exists.
resource_exists(ReqData, Context) ->
    Context1  = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    try
        Id = get_id(ContextQs),
        case {m_rsc:exists(Id, ContextQs), z_context:get(cat, ContextQs)} of
            {Exists, undefined} ->
                ?WM_REPLY(Exists, ContextQs);
            {true, Cat} ->
                ?WM_REPLY(m_rsc:is_a(Id, Cat, ContextQs), ContextQs);
            {false, _} ->
                ?WM_REPLY(false, ContextQs)
        end
    catch
        _:_ -> ?WM_REPLY(false, ContextQs)
    end.

%% @doc Check if the resource used to exist
previously_existed(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    IsGone = case get_id(Context1) of
                 Id when is_integer(Id) ->
                     m_rsc_gone:is_gone(Id, Context1);
                 _ -> false
             end,
    ?WM_REPLY(IsGone, Context1).


%% @doc Check if the current user is allowed to view the resource. 
is_authorized(ReqData, Context) ->
    controller_template:is_authorized(ReqData, Context).


%% @doc Show the page.  Add a noindex header when requested by the editor.
html(Context) ->
    ReqDataDump = m_req:m_to_list(#m{value=undefined},Context),
    lager:log(info, self(), io_lib:format("Dumping request data...~n~p", [ReqDataDump])),
    
    % The following will be moved into its own module, then triggered via notification
    % The following gets request stat info from the request data, enters it into Folsom
   
    Site = z_context:site(Context),
    ReqData = z_context:get_reqdata(Context),
    {{_,Month,Day},{Hour,_,_}} = erlang:localtime(),
    Path = m_req:get(path, ReqData),
    Referer = proplists:get_value("referer", wrq:req_headers(ReqData), none),
    UserAgent = m_req:get(user_agent, ReqData),
    Notification = #page_req{
    		 path=Path,
		 referer=Referer,
		 agent=UserAgent,
		 timestamp=erlang:localtime()
		 },
    z_notifier:notify(Notification, Context),
    
    folsom_metrics:notify({Site, req_by_hour, {Month,Day,Hour}}, {inc,1}, counter),
    folsom_metrics:notify({Site, req_by_day, {Month, Day, all}}, {inc,1}, counter),
    folsom_metrics:notify({Site, req_by_month, {Month, all, all}}, {inc,1}, counter),

    folsom_metrics:notify({Site, req_by_path, Path}, {inc,1}, counter),
    folsom_metrics:notify({Site, req_by_path_per_month, Path, {Month, all, all}}, {inc,1}, counter),
    folsom_metrics:notify({Site, req_by_path_per_day, Path, {Month, Day, all}}, {inc,1}, counter),
    folsom_metrics:notify({Site, req_by_path_per_hour, Path, {Month, Day, Hour}}, {inc,1}, counter),

    folsom_metrics:notify({Site, req_by_ref, Referer}, {inc,1}, counter),
    folsom_metrics:notify({Site, req_by_ref_per_month, Referer, {Month, all, all}}, {inc,1}, counter),
    folsom_metrics:notify({Site, req_by_ref_per_day, Referer, {Month, Day, all}}, {inc,1}, counter),
    folsom_metrics:notify({Site, req_by_ref_per_hour, Referer, {Month, Day, Hour}}, {inc,1}, counter),

    folsom_metrics:notify({Site, req_by_agent, UserAgent}, {inc,1}, counter),
    
    %% end of stats logging code

    Id = get_id(Context),
    Context1 = z_context:set_noindex_header(m_rsc:p(Id, seo_noindex, Context), Context),

	%% EXPERIMENTAL:
	%%
	%% When the 'cache_anonymous_maxage' flag is set then we enable simple page caching.
	%% This does not take into account any query args and vary headers.
	%% @todo Add the 'vary' headers to the cache key
	RenderArgs = [ {id, Id} | z_context:get_all(Context1) ],
	RenderFunc = fun() ->
		Template = z_context:get(template, Context1, "page.tpl"),
	    z_template:render(Template, RenderArgs, Context1)
	end,

	MaxAge = z_context:get(cache_anonymous_maxage, Context1),
	Html = case not z_auth:is_auth(Context1) of
		true when is_integer(MaxAge), MaxAge > 0 -> 
			QueryArgs = z_context:get_q_all(Context1),
		    z_depcache:memo(RenderFunc, {page_template_anonymous, RenderArgs, QueryArgs}, MaxAge, [Id], Context1);
		true when is_integer(MaxAge), MaxAge == 0 ->
			QueryArgs = z_context:get_q_all(Context1),
		    z_depcache:memo(RenderFunc, {page_template_anonymous, RenderArgs, QueryArgs}, 0, [], Context1);
		_ ->
			RenderFunc()
	end,
	%% End experimental.

	z_context:output(Html, Context1).


%% @doc Fetch the id from the request or the dispatch configuration.
%% @spec get_id(Context) -> int() | false
get_id(Context) ->
    ReqId = case z_context:get(id, Context) of
        undefined -> z_context:get_q("id", Context);
        ConfId -> ConfId
    end,
    case m_rsc:name_to_id(ReqId, Context) of
        {ok, RscId} -> RscId;
        _ -> undefined
    end.
