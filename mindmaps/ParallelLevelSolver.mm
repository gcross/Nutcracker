<map version="0.9.0">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node CREATED="1255293857359" ID="ID_368251692" MODIFIED="1255378815242" TEXT="Parallel Eigenstate Solver">
<node CREATED="1255294261251" ID="ID_650500725" MODIFIED="1255294265486" POSITION="right" TEXT="Input = Operator"/>
<node CREATED="1255294265909" ID="ID_178074555" MODIFIED="1255294273182" POSITION="right" TEXT="Output = Desired number of levels"/>
<node CREATED="1255294079404" ID="ID_1912381561" MODIFIED="1255294080930" POSITION="right" TEXT="Algorithm">
<node CREATED="1255294338716" ID="ID_702075377" MODIFIED="1255295024110" TEXT="Until we have found desired number of levels:">
<node CREATED="1255294852296" ID="ID_1965992474" MODIFIED="1255295106611" TEXT="Start with lowest energy = infinity, number of times seen = 0, state id = None, engine id = None"/>
<node CREATED="1255294864012" ID="ID_99149556" MODIFIED="1255294897270" TEXT="Until we have seen the lowest energy 3 times:">
<node CREATED="1255294354954" ID="ID_144307404" MODIFIED="1255294558002" TEXT="Spawn 3 tasks">
<node CREATED="1255293939267" ID="ID_1399898391" MODIFIED="1255294509613" TEXT="Task">
<node CREATED="1255293948001" ID="ID_1064411016" MODIFIED="1255294017278" TEXT="Input = Operator factory, old states"/>
<node CREATED="1255293956195" ID="ID_1315511641" MODIFIED="1255294051264" TEXT="Output = Energy, UUID of new state"/>
<node CREATED="1255294511525" ID="ID_1034902323" MODIFIED="1255294513629" TEXT="Algorithm">
<node CREATED="1255294514565" ID="ID_154011524" MODIFIED="1255294524206" TEXT="Run multiple sweeps, increasing bandwidth until convergence"/>
<node CREATED="1255294524915" ID="ID_478427138" MODIFIED="1255294540383" TEXT="Return state just before the last state"/>
</node>
</node>
</node>
<node CREATED="1255294545510" ID="ID_1109588939" MODIFIED="1255294928088" TEXT="Throw out all but the lowest energy states"/>
<node CREATED="1255294741732" ID="ID_294422038" MODIFIED="1255295070881" TEXT="If the remaining states have lower energy then the lowest energy, then set the lowest energy to the energy seen in these states and the number of times equal to the number of states kept"/>
<node CREATED="1255295071417" ID="ID_815144180" MODIFIED="1255295084738" TEXT="Clear all but one of the lowest energy states"/>
</node>
<node CREATED="1255295087850" ID="ID_1634698438" MODIFIED="1255295117268" TEXT="Tell the winning state&apos;s engine to write the state to the database"/>
</node>
</node>
<node CREATED="1255378816332" ID="ID_1990615655" MODIFIED="1255378818945" POSITION="left" TEXT="Components">
<node CREATED="1255378821336" ID="ID_520499371" MODIFIED="1255379010966" TEXT="Remote state accessor">
<node CREATED="1255378832524" ID="ID_1631309970" MODIFIED="1255378840979" TEXT="Referenceable"/>
</node>
<node CREATED="1255379004402" ID="ID_1040199017" MODIFIED="1255379255501" TEXT="Sweeper">
<node CREATED="1255379225872" ID="ID_600710240" MODIFIED="1255379233899" TEXT="Given:  references to old states"/>
<node CREATED="1255379234320" ID="ID_1921402947" MODIFIED="1255379330753" TEXT="Returns via. deferred:  new state (fetch reference + ID) + new energy"/>
<node CREATED="1255379261173" ID="ID_1731459596" MODIFIED="1255379287551" TEXT="Client function">
<node CREATED="1255379292007" ID="ID_1415745512" MODIFIED="1255379297103" TEXT="Static method on Server Task?"/>
<node CREATED="1255379304391" ID="ID_974644893" MODIFIED="1255379346322" TEXT="Fetches states from remote references unless ID is in cache"/>
</node>
<node CREATED="1255379266402" ID="ID_159945275" MODIFIED="1255379394629" TEXT="Server Task class">
<node CREATED="1255379395307" ID="ID_631628107" MODIFIED="1255379402070" TEXT="Handed to the scheduler"/>
<node CREATED="1255379402427" ID="ID_778804969" MODIFIED="1255379418007" TEXT="Gives client the information it needs, plus a reference to send its result"/>
<node CREATED="1255379419004" ID="ID_1664031000" MODIFIED="1255379438328" TEXT="Upon result arrival, hands result to its own deferred"/>
</node>
</node>
<node CREATED="1255379097851" ID="ID_576855152" MODIFIED="1255379100115" TEXT="Level solver class">
<node CREATED="1255379583946" ID="ID_1920341251" MODIFIED="1255379594082" TEXT="Delegates to sweepers the task of performing trials"/>
<node CREATED="1255379594618" ID="ID_1844608840" MODIFIED="1255379624740" TEXT="Collects trial results, does housecleaning, and then either declares victory or runs more trials"/>
</node>
<node CREATED="1255379628124" ID="ID_1886726669" MODIFIED="1255379630292" TEXT="Eigensolver class"/>
</node>
</node>
</map>
