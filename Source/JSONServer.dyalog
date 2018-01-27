:Class JSONServer
    :Field Public AcceptFrom←⍬    ⍝ IP addressed to accept requests from - empty means all
    :Field Public Port←8080       ⍝
    :Field Public CodeLocation←#
    :Field Public InitializeFn←'Initialize' ⍝ name of the application "bootstrap" function
    :Field Public AllowedFns←''   ⍝ list of functions to be "exposed" by this service, can be a vector or comma-delimited list of function names
    :Field Public ConfigFile←''
    :Field Public Logging←0       ⍝ turn logging on/off
    :Field Public HtmlInterface←1
    :Field Public Debug←0

⍝ Fields related to running a secure server (to be implemented)
    :Field Public Secure←0
⍝    :Field Public RootCertDir
⍝    :Field Public SSLValidation
⍝    :Field Public ServerCertFile
⍝    :Field Public ServerKeyFile

    :Field Folder←''
    :Field _configLoaded←0
    :Field _stop←0               ⍝ set to 1 to stop server
    :Field _started←0
    :Field _stopped←1


    ∇ make
      :Access public
      :Implements constructor
    ∇

    ∇ make1 args;port;loc
      :Access public
      :Implements constructor
    ⍝ args[1] port to listen on
    ⍝     [2] charvec function folder or ref to codelocation
      (Port CodeLocation)←2↑args,(≢,args)↓Port CodeLocation
    ∇

    ∇ r←Run args;msg;rc
      :Access shared public
      :Trap 0
          (rc msg)←(r←⎕NEW ⎕THIS args).Start
      :Else
          (r rc msg)←'' ¯1(⊃⎕DMX.EM)
      :EndTrap
      r←(r(rc msg))
    ∇

    ∇ (rc msg)←Start
      :Access public
     
      :If _started
          CheckRC(rc msg)←¯1 'Server thinks it''s already started'
      :EndIf
     
      :If _stop
          CheckRC(rc msg)←¯1 'Server is in the process of stopping'
      :EndIf
     
      CheckRC(rc msg)←LoadConfiguration
     
      :If 1=≡,AllowedFns
          AllowedFns←(⊂'')~⍨{⍵⊆⍨','≠⍵}',',AllowedFns
      :EndIf
      AllowedFns←,(⊆⍣(~0∊⍴AllowedFns)),AllowedFns
      CheckRC(rc msg)←CheckPort
      CheckRC(rc msg)←LoadConga
      CheckRC(rc msg)←CheckCodeLocation
      CheckRC(rc msg)←StartServer
      Log'JSONServer started on port ',⍕Port
      :If HtmlInterface
          Log'Click http',(~Secure)↓'s://localhost:',(⍕Port),' to access web interface'
      :EndIf
    ∇

    ∇ (rc msg)←Stop;ts
      :Access public
      :If _stop
          CheckRC(rc msg)←¯1 'Server is already stopping'
      :EndIf
      :If ~_started
          CheckRC(rc msg)←¯1 'Server is not running'
      :EndIf
      ts←⎕AI[3]
      _stop←1
      Log'Stopping server...'
      :While ~_stopped
          :If 10000<⎕AI[3]-ts
              CheckRC(rc msg)←¯1 'Server seems stuck'
          :EndIf
      :EndWhile
      _started←_stop←0
    ∇

    ∇ r←Running
      :Access public
      r←~_stop
    ∇

    ∇ (rc msg)←CheckPort;p
      (rc msg)←3('Invalid port: ',∊⍕Port)
      ExitIf 0=p←⊃⊃(//)⎕VFI⍕Port
      ExitIf{(⍵>32767)∨(⍵<1)∨⍵≠⌊⍵}p
      (rc msg)←0 ''
    ∇

    ∇ (rc msg)←LoadConfiguration;config;params
      :Access public
      ⍝!!! wip
      (rc msg)←0 ''
      :If ~0∊⍴ConfigFile
          :Trap 0/0
              :If ⎕NEXISTS ConfigFile
                  config←⎕JSON⊃⎕NGET ConfigFile
                  params←{⍵/'_'≠⊃¨⍵}⎕NL ¯2.2
                  ∘∘∘
                  config.⎕NL ¯2
              :Else
                  →0⊣(rc msg)←6('Configuation file "',ConfigFile,'" not found')
              :EndIf
              _configLoaded←1
          :EndTrap
      :EndIf
    ∇


    ∇ (rc msg)←LoadConga;dyalog
      (rc msg)←0 ''
     
      :If 0=#.⎕NC'Conga'
          dyalog←{⍵,'/'↓⍨'/\'∊⍨¯1↑⍵}2 ⎕NQ'.' 'GetEnvironment' 'DYALOG'
          :Trap 0
              'Conga'#.⎕CY dyalog,'ws/conga'
          :Else
              (rc msg)←1 'Unable to copy Conga'
              →0
          :EndTrap
      :EndIf
     
      :Trap 999 ⍝ Conga.Init signals 999 on error
          #.DRC←#.Conga.Init''
      :Else
          (rc msg)←2 'Unable to initialize Conga'
          →0
      :EndTrap
    ∇

    ∇ (rc msg)←CheckCodeLocation;root;folder;m;res
      (rc msg)←0 ''
      :If 0∊⍴CodeLocation
          CheckRC(rc msg)←4 'CodeLocation is empty!'
      :EndIf
      :Select ⊃{⎕NC'⍵'}CodeLocation ⍝ need dfn because CodeLocation is a field and will always be nameclass 2
      :Case 9 ⍝ reference, just use it
      :Case 2 ⍝ variable, should be file path
          :If isRelPath CodeLocation
              :If 'CLEAR WS'≡⎕WSID
                  root←⊃1 ⎕NPARTS''
              :Else
                  root←⊃1 ⎕NPARTS ⎕WSID
              :EndIf
          :Else
              root←''
          :EndIf
          folder←∊1 ⎕NPARTS root,CodeLocation
          :Trap 0
              :If 1≠1 ⎕NINFO folder
                  CheckRC(rc msg)←5('CodeLocation "',(∊⍕CodeLocation)'," is not a folder.')
              :EndIf
          :Case 22 ⍝ file name error
              CheckRC(rc msg)←6('CodeLocation "',(∊⍕CodeLocation)'," was not found.')
          :Else    ⍝ anything else
              CheckRC(rc msg)←7((⎕DMX.(EM,' (',Message,') ')),'occured when validating CodeLocation "',(∊⍕CodeLocation),'"')
          :EndTrap
          CodeLocation←⍎'CodeLocation'#.⎕NS''
          (rc msg)←CodeLocation LoadFromFolder Folder←folder
      :Else
          CheckRC(rc msg)←5 'CodeLocation is not valid, it should be either a namespace/class reference or a file path'
      :EndSelect
     
      :If ~0∊⍴InitializeFn  ⍝ initialization function specified?
          :If 3=CodeLocation.⎕NC InitializeFn ⍝ does it exist?
              :If 1 0 0≡⊃CodeLocation.⎕AT InitializeFn ⍝ with the correct syntax?
                  res←,⊆CodeLocation⍎InitializeFn        ⍝ run it
                  CheckRC(rc msg)←2↑res,(⍴res)↓¯1('"',(⍕CodeLocation),'.',InitializeFn,'" did not return a 0 return code')
              :Else
                  CheckRC(rc msg)←8('"',(⍕CodeLocation),'.',InitializeFn,'" is not a niladic result-returning function')
              :EndIf
          :EndIf
      :EndIf
    ∇

    ∇ (rc msg)←StartServer;r
      msg←'Unable to start server'
      :If 98 10048∊⍨rc←1⊃r←#.DRC.Srv'' ''Port'http' 10000 ⍝ 98=Linux, 10048=Windows
          →0⊣msg←'Server could not start - port ',(⍕Port),' is already in use'
      :ElseIf 0=rc
          (_started _stopped)←1 0
          ServerName←2⊃r
          {}#.DRC.SetProp'.' 'EventMode' 1 ⍝ report Close/Timeout as events
          {}#.DRC.SetProp ServerName'FIFOMode' 1
          {}#.DRC.SetProp ServerName'DecodeBuffers' 1
          Connections←#.⎕NS''
          RunServer
          msg←''
      :EndIf
    ∇

    ∇ RunServer
      {}Server&⍬
    ∇

    ∇ Server arg;wres;rc;obj;evt;data
      :While ~_stop
          wres←#.DRC.Wait ServerName 5000 ⍝ Wait for WaitTimeout before timing out
          ⍝ wres: (return code) (object name) (command) (data)
          (rc obj evt data)←4↑wres
          :Select rc
          :Case 0
              :Select evt
              :Case 'Error'
                  :If ServerName≡obj
                      Stop←1
                  :Else
                      Connections.⎕EX obj
                  :EndIf
                  :If 0≠4⊃wres
                      Log'RunServer: DRC.Wait reported error ',(⍕#.Conga.Error 4⊃wres),' on ',2⊃wres
                  :EndIf
     
              :Case 'Connect'
                  obj Connections.⎕NS''
     
              :CaseList 'HTTPHeader' 'HTTPTrailer' 'HTTPChunk' 'HTTPBody'
                  {}(Connections⍎obj){t←⍺ HandleRequest ⍵ ⋄ ⎕EX t/⍕⍺}&wres
     
              :Case 'Timeout'
     
              :Else ⍝ unhandled event
                  2 Log'Unhandled Conga event:'
                  2 Log⍕res
              :EndSelect ⍝ evt
     
          :Case 1010 ⍝ Object Not found
             ⍝ Log'Object ''',ServerName,''' has been closed - Web Server shutting down'
              →0
     
          :Else
              Log'Conga wait failed:'
              Log wres
          :EndSelect ⍝ rc
      :EndWhile
      {}#.DRC.Close ServerName
      _stopped←1
    ∇

    ∇ r←ns HandleRequest req;data;evt;obj;rc
      (rc obj evt data)←req
      r←0
      :Hold obj
          :If Debug
              ∘∘∘
          :EndIf
          :Select evt
          :Case 'HTTPHeader'
              ns.Req←⎕NEW Request data
              :If Logging
                  ⎕←('G⊂9999/99/99 @ 99:99:99⊃'⎕FMT 100⊥6↑⎕TS)data
              :EndIf
          :Case 'HTTPBody'
              ns.Req.ProcessBody data
              :If Logging
                  ⎕←('G⊂9999/99/99 @ 99:99:99⊃'⎕FMT 100⊥6↑⎕TS)data
              :EndIf
          :Case 'HTTPChunk'
              ns.Req.ProcessChunk data
          :Case 'HTTPTrailer'
              ns.Req.ProcessTrailer data
          :EndSelect
     
          :If ns.Req.Complete
              :If ns.Req.Response.Status=200
                  HandleJSONRequest ns
              :EndIf
              r←Respond ns.Req.Response
          :EndIf
      :EndHold
    ∇

    ∇ HandleJSONRequest ns;payload;fn
      ExitIf HtmlInterface∧ns.Req.Page≡'/favicon.ico'
      :If 0∊⍴fn←1↓'.'@('/'∘=)ns.Req.Page
          ExitIf('No function specified')ns.Req.FailIf~HtmlInterface∧'get'≡ns.Req.Method
          ns.Req.Response.Headers←1 2⍴'Content-Type' 'text/html'
          ns.Req.Response.JSON←HtmlPage
          →0
      :EndIf
     
      ExitIf('Could not locate method "',fn,'"')ns.Req.FailIf{0::1 ⋄ 3≠CodeLocation.⎕NC ⍵}fn
     
      :Trap 0
          payload←{0∊⍴⍵:⍵ ⋄ 0 ⎕JSON ⍵}ns.Req.Body
      :Else
          →0⍴⍨'Could not parse payload as JSON'ns.Req.FailIf 1
      :EndTrap
     
      :If ~0∊⍴AllowedFns
          ExitIf('"',fn,'" is not an allowed method')ns.Req.FailIf~(⊂fn)∊AllowedFns
      :EndIf
     
      ExitIf('"',fn,'" is not a monadic result-returning function')ns.Req.FailIf 1 1 0≢⊃CodeLocation.⎕AT fn
     
      :Trap 0
          payload←(CodeLocation⍎fn)payload
      :Else
          ns.Req.Response.JSON←1 ⎕JSON ⎕DMX.(EM Message)
          ExitIf('Error running method "',fn,'"')ns.Req.FailIf 1
      :EndTrap
     
      :Trap 0
          ns.Req.Response.JSON←1 ⎕JSON payload
      :Else
          ExitIf'Could not format payload as JSON'ns.Req.FailIf 1
      :EndTrap
    ∇

    ∇ r←Respond res;status;z
      status←(⊂'HTTP/1.1'),res.((⍕Status)StatusText)
      :If res.Status≠200 ⋄ res.Headers←2 2⍴'content-type' 'text/html' 'content-length'(⍕res.JSON) ⋄ :EndIf
      :If Logging
          ⎕←('G⊂9999/99/99 @ 99:99:99⊃'⎕FMT 100⊥6↑⎕TS)status res.Headers res.JSON
      :EndIf
      :If 0≠1⊃z←#.DRC.Send obj(status,res.Headers res.JSON)1
          Log'Conga error when sending response'
          Log⍕z
      :EndIf
      r←1
    ∇

    :Class Request
        :Field Public Instance Complete←0        ⍝ do we have a complete request?
        :Field Public Instance Input←''
        :Field Public Instance Host←''           ⍝ host header field
        :Field Public Instance Headers←0 2⍴⊂''   ⍝ HTTPRequest header fields (plus any supplied from HTTPTrailer event)
        :Field Public Instance Method←''         ⍝ HTTP method (GET, POST, PUT, etc)
        :Field Public Instance Page←''           ⍝ Requested URI
        :Field Public Instance Body←''           ⍝ body of the request
        :Field Public Instance PeerAddr←''       ⍝ client IP address
        :Field Public Instance PeerCert←0 0⍴⊂''  ⍝ client certificate
        :Field Public Instance HTTPVersion←''
        :Field Public Instance Cookies←0 2⍴⊂''
        :Field Public Instance CloseConnection←0
        :Field Public Instance Response

        GetFromTable←{(⍵[;1]⍳⊂lc ,⍺)⊃⍵[;2],⊂''}
        split←{p←(⍺⍷⍵)⍳1 ⋄ ((p-1)↑⍵)(p↓⍵)} ⍝ Split ⍵ on first occurrence of ⍺
        lc←(819⌶)
        begins←{⍺≡(⍴⍺)↑⍵}

        ∇ {r}←{a}FailIf w
          :Access public
          r←a{⍺←'' ⋄ ⍵:⍵⊣Response.(Status StatusText)←400('Bad Request',(3×0∊⍴⍺)↓' - ',⍺) ⋄ ⍵}w
        ∇

        ∇ make args;query;origin
          :Access public
          :Implements constructor
          (Method Input HTTPVersion Headers)←args
          Headers[;1]←lc Headers[;1]  ⍝ header names are case insensitive
          Method←lc Method
         
          Response←⎕NS''
          Response.(Status StatusText Headers JSON)←200 'OK'(1 2⍴'Content-Type' 'application/json')''
         
          Host←'host'GetFromTable Headers
          (Page query)←'?'split Input
          Page←PercentDecode Page
          :If Complete←('get'≡Method)∨('content-length' GetFromTable Headers)≡,'0'
          :AndIf ##.HtmlInterface∧~(⊂Page)∊(,'/')'/favicon.ico'
              →0⍴⍨'(Request method should be POST)'FailIf'post'≢Method
              →0⍴⍨'(Bad URI)'FailIf'/'≠⊃Page
              →0⍴⍨'(Content-Type should be application/json)'FailIf'application/json'≢lc'content-type'GetFromTable Headers
          :EndIf
          →0⍴⍨'(Cannot accept query parameters)'FailIf~0∊⍴query
        ∇


        ∇ ProcessBody args
          :Access public
          Body←args
          Complete←1
        ∇

        ∇ ProcessChunk args
          :Access public
        ⍝ args is [1] chunk content [2] chunk-extension name/value pairs (which we don't expect and won't process)
          Body,←1⊃args
        ∇

        ∇ ProcessTrailer args;inds;mask
          :Access public
          args[;1]←lc args[;1]
          mask←(≢Headers)≥inds←Headers[;1]⍳args[;1]
          Headers[mask/inds;2]←mask/args[;2]
          Headers⍪←(~mask)⌿args
          Complete←1
        ∇

        ∇ r←PercentDecode r;rgx;rgxu;i;j;z;t;m;⎕IO;lens;fill
          :Access public shared
        ⍝ Decode a Percent Encoded string https://en.wikipedia.org/wiki/Percent-encoding
          ⎕IO←0
          ((r='+')/r)←' '
          rgx←'[0-9a-fA-F]'
          rgxu←'%[uU]',(4×⍴rgx)⍴rgx ⍝ 4 characters
          r←(rgxu ⎕R{{⎕UCS 16⊥⍉16|'0123456789ABCDEF0123456789abcdef'⍳⍵}2↓⍵.Match})r
          :If 0≠⍴i←(r='%')/⍳⍴r
          :AndIf 0≠⍴i←(i≤¯2+⍴r)/i
              z←r[j←i∘.+1 2]
              t←'UTF-8'⎕UCS 16⊥⍉16|'0123456789ABCDEF0123456789abcdef'⍳z
              lens←⊃∘⍴¨'UTF-8'∘⎕UCS¨t  ⍝ UTF-8 is variable length encoding
              fill←i[¯1↓+\0,lens]
              r[fill]←t
              m←(⍴r)⍴1 ⋄ m[(,j),i~fill]←0
              r←m/r
          :EndIf
        ∇

        ∇ r←GetHeader name
          :Access Public Instance
          r←(lc name)GetFromTable Headers
        ∇

    :EndClass

    :Section Utilities
    ExitIf←→⍴∘0
    CheckRC←ExitIf(0∘≠⊃)
    ∇ r←isRelPath w
      r←{{~'/\'∊⍨(⎕IO+2×('Win'≡3↑⊃#.⎕WG'APLVersion')∧':'∊⍵)⊃⍵}3↑⍵}w
    ∇
    lc←(819⌶)
    Log←{⎕←⍕⍵}

    ∇ (rc msg)←{root}LoadFromFolder path;type;name;nsName;parts;ns;files;folders;file;folder;ref;r;m
      :Access public shared
    ⍝ Loads an APL "project" folder
      (rc msg)←0 ''
      root←{6::⍵ ⋄ root}#
      files←⊃{(⍵=2)/⍺}/0 1(⎕NINFO⍠1)∊1 ⎕NPARTS path,'/*.dyalog'
      folders←⊃{(⍵=1)/⍺}/0 1(⎕NINFO⍠1)∊1 ⎕NPARTS path,'/*'
      :For file :In files
          ⎕SE.SALT.Load file,' -target=',⍕root
      :EndFor
      :For folder :In folders
          nsName←2⊃1 ⎕NPARTS folder
          ref←0
          :Select root.⎕NC⊂nsName
          :Case 9.1 ⍝ namespace
              ref←root⍎nsName
          :Case 0   ⍝ not defined
              ref←⍎nsName root.⎕NS''
          :Else     ⍝ oops
              msg,←'"',folder,'" cannot be mapped to a valid namespace name',⎕UCS 13
          :EndSelect
          :If ref≢0
              (r m)←ref LoadFromFolder folder
              r←rc⌈r
              msg,←m
          :EndIf
      :EndFor
      msg←¯1↓msg
    ∇
    :EndSection

    :Section HTML
    ∇ r←ScriptFollows;n
      :Access public shared
      n←2
      r←{⍵/⍨'⍝'≠⊃¨⍵}{1↓¨⍵/⍨∧\'⍝'=⊃¨⍵}{⍵{((∨\⍵)∧⌽∨\⌽⍵)/⍺}' '≠⍵}¨(1+n⊃⎕LC)↓↓(180⌶)n⊃⎕XSI
      r←2↓∊(⎕UCS 13 10)∘,¨r
    ∇

    ∇ r←HtmlPage
      :Access public shared
      r←ScriptFollows
⍝<!DOCTYPE html>
⍝<html>
⍝<head>
⍝<meta content="text/html; charset=utf-8" http-equiv="Content-Type">
⍝<title>JSONServer</title>
⍝</head>
⍝<body>
⍝<fieldset>
⍝  <legend>Request</legend>
⍝  <form id="myform">
⍝    <table>
⍝      <tr>
⍝        <td><label for="function">Method to Execute:</label></td>
⍝        <td><input id="function" name="function" type="text"></td>
⍝      </tr>
⍝      <tr>
⍝        <td><label for="payload">JSON Data:</label></td>
⍝        <td><textarea id="payload" cols="100" name="payload" rows="10"></textarea></td>
⍝      </tr>
⍝      <tr>
⍝        <td colspan="2"><button onclick="doit()" type="button">Send</button></td>
⍝      </tr>
⍝    </table>
⍝  </form>
⍝</fieldset>
⍝<fieldset>
⍝  <legend>Response</legend>
⍝  <div id="result">
⍝  </div>
⍝</fieldset>
⍝<script>
⍝function doit() {
⍝  document.getElementById("result").innerHTML = "";
⍝  var xhttp = new XMLHttpRequest();
⍝  var fn = document.getElementById("function").value;
⍝  fn = (0 == fn.indexOf('/')) ? fn : '/' + fn;
⍝
⍝  xhttp.open("POST", fn, true);
⍝  xhttp.setRequestHeader("Content-Type", "application/json");
⍝
⍝  xhttp.onreadystatechange = function() {
⍝    if (this.readyState == 4){
⍝//      var resp = "";
⍝      if (this.status == 200) {
⍝        var resp = "<pre><code>" + this.responseText + "</code></pre>";
⍝      } else {
⍝        var resp = "<span style='color:red;'>" + this.statusText + "</span>";
⍝      }
⍝      document.getElementById("result").innerHTML = resp;
⍝    }
⍝  }
⍝  xhttp.send(document.getElementById("payload").value);
⍝}
⍝</script>
⍝</body>
⍝</html>
    ∇
    :EndSection

:EndClass
