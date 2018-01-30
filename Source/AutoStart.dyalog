 {ref}←AutoStart;empty;validParams;mask;values;params;param;value;rc;msg;getEnv;NoSession;ts;t;commits;n
⍝ JSONServer automatic startup
⍝ General logic:
⍝   Command line parameters take priority over configuration file which takes priority over default

 empty←0∊⍴
 getEnv←{2 ⎕NQ'.' 'GetEnvironment'⍵}

 validParams←'ConfigFile' 'CodeLocation' 'Port' 'InitializeFn' 'AllowedFns' ⍝ to be added - 'Secure' 'RootCertDir' 'SSLValidation' 'ServerCertFile' 'ServerKeyFile'
 mask←~empty¨values←getEnv¨validParams
 params←mask⌿validParams,⍪values
 NoSession←~empty getEnv'NoSession'

 ref←'No server running'
 :If ~empty params
     ref←⎕NEW #.JSONServer

     :For (param value) :In ↓params  ⍝ need to load one at a time because params can override what's in the configuration file
         param(ref{⍺⍺⍎⍺,'←⍵'})value
         :If 'ConfigFile'≡param
             :If 0≠⊃(rc msg)←ref.LoadConfiguration value
                 ∘∘∘
             :EndIf
         :EndIf
     :EndFor

     :If 0≠⊃(rc msg)←ref.Start
         ('Unable to start server - ',msg)⎕SIGNAL 16
     :EndIf

     :If NoSession∨'R'=3⊃#.⎕WG'APLVersion' ⍝ no session or runtime?
         :While ref.Running
             {}⎕DL 10
         :EndWhile
     :EndIf
 :EndIf
