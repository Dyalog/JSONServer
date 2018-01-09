 {ref}←AutoStart;beginsWith;cio;splitFirst;validParams;commandLineArgs;hits;inds;mask;i;rc;msg;configFile;param;value
⍝ JSONServer automatic startup
⍝ General logic:
⍝   Command line parameters take priority over configuration file which takes priority over default


 cio←{(819⌶⍺)⍺⍺(819⌶⍵)} ⍝ case insensitive operator
 splitFirst←{⍵{((⍵-1)↑⍺)(⍵↓⍺)}⍵⍳⍺}
 validParams←'ConfigFile' 'CodeLocation' 'Port' 'InitializeFn' 'AllowedFns' ⍝ to be added - 'Secure' 'RootCertDir' 'SSLValidation' 'ServerCertFile' 'ServerKeyFile'

 commandLineArgs←1↓2 ⎕NQ'.' 'GetCommandLineArgs'
 commandLineArgs/⍨←~'+-'∊⍨⊃¨commandLineArgs ⍝ remove command line options (begin with + or -)
 commandLineArgs/⍨←'='∊¨commandLineArgs     ⍝ keep only command line options that have an =

 ref←''
 :If ~0∊⍴commandLineArgs

     commandLineArgs←↑'='splitFirst¨commandLineArgs
     mask←(≢validParams)≥inds←validParams(⍳cio)commandLineArgs[;1]
     commandLineArgs←mask⌿commandLineArgs
     commandLineArgs[;1]←validParams[mask/inds]
     commandLineArgs←(⊂⍋mask/inds)⌷[1]commandLineArgs

     :If ~0∊⍴commandLineArgs
         ref←⎕NEW #.JSONServer

         :For (param value) :In ↓commandLineArgs
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
         :If 'R'=3⊃#.⎕WG'APLVersion' ⍝ runtime?
             :While ref.Running
                 {}⎕DL 10
             :EndWhile
         :EndIf
     :EndIf
 :EndIf
