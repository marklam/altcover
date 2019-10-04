param([string]$ACV="0.0.0.0")
$x = "./_Reports/PesterFSharpTypesDotNetRunner.xml"
$o = "./Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.1"
$i = "./_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.1"
dir "./_Packaging/*.*" | % { if ( -not($_.Name -like "*.nupkg")) { del -force $_.FullName }}
if (Test-Path $x) { del -force $x }

Describe "Invoke-Altcover" {
    It "instruments and collects" {
        if (Test-Path $o) {
            Remove-Item -Force -Recurse $o
        }
        Invoke-AltCover -XmlReport $x -OutputDirectory  $o -InputDirectory $i -AssemblyFilter "Adapter" -InformationAction Continue
        $o | Should -Exist
        $x | Should -Exist
        $xm = [xml](Get-Content $x)
        [string]::Join(" ", $xm.coverage.module.method.name) | Should -Be "main returnFoo returnBar testMakeUnion as_bar get_MyBar Invoke .ctor makeThing testMakeThing bytes"
        [string]::Join(" ", $xm.coverage.module.method.seqpnt.visitcount) | Should -Be "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"

        $w = ""
        Invoke-AltCover -Runner -RecorderDirectory $o -WarningVariable w
        $xm = [xml](Get-Content $x)
        [string]::Join(" ", $xm.coverage.module.method.name) | Should -Be "main returnFoo returnBar testMakeUnion as_bar get_MyBar Invoke .ctor makeThing testMakeThing bytes"
        [string]::Join(" ", $xm.coverage.module.method.seqpnt.visitcount) | Should -Be "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
        $w | Should -Be "A total of 0 visits recorded"

        $summary = Invoke-AltCover  -InformationAction Continue -Runner -RecorderDirectory $o -WorkingDirectory "./Sample2" -Executable "dotnet" -CommandLine @("test", "--no-build", "--configuration", "Debug",  "sample2.core.fsproj")
        $xm2 = [xml](Get-Content $x)
        Remove-Item -Force -Recurse $o
        [string]::Join(" ", $xm2.coverage.module.method.seqpnt.visitcount) | Should -Be "0 1 1 1 1 1 1 0 0 0 0 0 0 0 2 1 1 1"
        $summary.Replace("`r", [String]::Empty).Replace("`n", "|") | Should -Be "Visited Classes 4 of 7 (57.14)|Visited Methods 7 of 11 (63.64)|Visited Points 10 of 18 (55.56)|"
    }

    It "Fails on garbage" {
        $saved = [System.Console]::Error
        $stderr = new-object System.IO.StringWriter @()
        [System.Console]::SetError($stderr)
        try 
        {
          $ev = ""
          Invoke-AltCover -XmlReport $x -OutputDirectory  $o -InputDirectory "./NoneSuch/xunit-dotnet/bin/Debug/netcoreapp2.0" -InPlace -ErrorVariable ev -ErrorAction SilentlyContinue
        }
		catch {
          $ev | Should -BeTrue
          $stderr.ToString()  | Should -BeTrue	      	
		}
        finally
        {
            [System.Console]::SetError($saved)     
        }

        $ev | Should -BeTrue
        $stderr.ToString()  | Should -BeTrue
    }

    It "Reports the version" {        
        $version = Invoke-AltCover -Version -InformationAction Continue 6>&1
        $version.ToString().Trim() | Should -Be ("AltCover version " + $ACV)
    }
}

Describe "ConvertTo-XDocument" {
    It "converts" {
        $xml = [xml](Get-Content "./Tests/Sample1WithNCover.xml")
        $xd = $xml | ConvertTo-XDocument
        $xd | Should -BeOfType "System.Xml.Linq.XDocument"
        $header = $xd.Declaration.ToString().Replace(" standalone=`"`"", "") + "`n" 
        $sw = new-object System.IO.StringWriter @()
        $settings = new-object System.Xml.XmlWriterSettings @()
        $settings.Indent = $true
        $settings.IndentChars = "  "
        $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
        $xml.WriteTo($xw)
        $xw.Close()
        ($header + $xd.ToString()).Replace("`r", "") | Should -Be $sw.ToString().Replace("`r", "")
    }

    It "Round Trips" {
        $xml = [xml]"<document/>"
        $xd = $xml | ConvertTo-XDocument
        $xd.ToString() | Should -Be $xml.OuterXml
        $x2 = $xd | ConvertTo-XmlDocument
        $x2.OuterXml | Should -Be $xml.OuterXml
    }
}

Describe "ConvertTo-XmlDocument" {
    It "converts" {
        $xd = [System.Xml.Linq.XDocument]::Load("./Tests/Sample1WithNCover.xml")
        $xml = $xd | ConvertTo-XmlDocument
        $xml | Should -BeOfType "System.Xml.XmlDocument"
        $sw = new-object System.IO.StringWriter @()
        $settings = new-object System.Xml.XmlWriterSettings @()
        $settings.Indent = $true
        $settings.IndentChars = "  "
        $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
        $xml.WriteTo($xw)
        $xw.Close()
        $header = $xd.Declaration.ToString() + "`n"
        $sw.ToString().Replace("`r", "") | Should -Be ($header + $xd.ToString()).Replace("`r", "")
    }
}

Describe "ConvertTo-Lcov" {
    It "Converts OpenCover Data" {
        ConvertTo-LCov -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/OpenCover.lcov"
        $expected = @"
TN:
SF:altcover/Sample1/Program.cs
FN:11,System.Void TouchTest.Program::Main(System.String[])
FNDA:1,System.Void TouchTest.Program::Main(System.String[])
FNF:1
FNH:1
BRDA:13,0,0,1
BRDA:13,0,1,-
BRF:2
BRH:1
DA:11,1
DA:12,1
DA:13,1
DA:13,1
DA:14,1
DA:15,1
DA:15,1
DA:15,1
DA:16,1
DA:18,0
DA:19,0
DA:19,0
DA:20,0
DA:21,1
LH:10
LF:14
end_of_record
"@
        $got = [String]::Join("`n", (Get-Content "./_Packaging/OpenCover.lcov"))
        $got | Should -Be $expected.Replace("`r", "")
    }

  It "Converts NCover Data" {
      $lines = (Get-Content "./Tests/Sample1WithNCover.xml") | % { $_.Replace('excluded="true"', 'excluded="false"')}
      $lines | Set-Content "./_Packaging/NCover.lcov.xml"
      ConvertTo-LCov -InputFile "./_Packaging/NCover.lcov.xml" -OutputFile "./_Packaging/NCover.lcov"
      $expected = [String]::Join("`n", (Get-Content "./Tests/NCoverBugFix.lcov"))
      $got = [String]::Join("`n", (Get-Content "./_Packaging/NCover.lcov"))
      $got | Should -Be $expected.Replace("`r", "")
  }

  It "Converts Real NCover Data" {
    $ev = ""
    [xml](Get-Content "./Tests/GenuineNCover158.Xml") | ConvertTo-LCov -OutputFile "./_Packaging/NCover158.lcov" -ErrorVariable ev
    $ev | Should -BeFalse
  }
}

Describe "ConvertTo-Cobertura" {
  It "Converts OpenCover Data" {
    $x = ConvertTo-Cobertura -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/OpenCover.cobertura"
    $coverage = $x.Descendants("coverage")
    $v = $coverage.Attribute("version").Value
    $t = $coverage.Attribute("timestamp").Value

    $expected = @"
<?xml version="1.0" encoding="utf-8" standalone="no"?>
<!DOCTYPE coverage SYSTEM "http://cobertura.sourceforge.net/xml/coverage-04.dtd">
<coverage line-rate="0.7142857142857143" branch-rate="0.66666666666666663" lines-covered="10" lines-valid="14" branches-covered="2" branches-valid="3" complexity="2" version="$v" timestamp="$t">
  <sources>
    <source>altcover\Sample1</source>
  </sources>
  <packages>
    <package name="Sample1" line-rate="0.7142857142857143" branch-rate="0.66666666666666663" complexity="2">
      <classes>
        <class name="TouchTest.Program" filename="altcover/Sample1/Program.cs" line-rate="0.7142857142857143" branch-rate="0.66666666666666663" complexity="2">
          <methods>
            <method name="Main" signature="System.Void System.String[])" line-rate="0.7142857142857143" branch-rate="0.66666666666666663" complexity="2">
              <lines>
                <line number="11" hits="1" branch="false" />
                <line number="12" hits="1" branch="false" />
                <line number="13" hits="1" branch="false" />
                <line number="13" hits="1" branch="true" condition-coverage="50% (1/2)">
                  <conditions>
                    <condition number="10" type="jump" coverage="50%" />
                  </conditions>
                </line>
                <line number="14" hits="1" branch="false" />
                <line number="15" hits="1" branch="false" />
                <line number="15" hits="1" branch="false" />
                <line number="15" hits="1" branch="false" />
                <line number="16" hits="1" branch="false" />
                <line number="18" hits="0" branch="false" />
                <line number="19" hits="0" branch="false" />
                <line number="19" hits="0" branch="false" />
                <line number="20" hits="0" branch="false" />
                <line number="21" hits="1" branch="false" />
              </lines>
            </method>
          </methods>
          <lines>
            <line number="11" hits="1" branch="false" />
            <line number="12" hits="1" branch="false" />
            <line number="13" hits="1" branch="false" />
            <line number="13" hits="1" branch="true" condition-coverage="50% (1/2)">
              <conditions>
                <condition number="10" type="jump" coverage="50%" />
              </conditions>
            </line>
            <line number="14" hits="1" branch="false" />
            <line number="15" hits="1" branch="false" />
            <line number="15" hits="1" branch="false" />
            <line number="15" hits="1" branch="false" />
            <line number="16" hits="1" branch="false" />
            <line number="18" hits="0" branch="false" />
            <line number="19" hits="0" branch="false" />
            <line number="19" hits="0" branch="false" />
            <line number="20" hits="0" branch="false" />
            <line number="21" hits="1" branch="false" />
          </lines>
        </class>
      </classes>
    </package>
  </packages>
</coverage>
"@
    $got = [String]::Join("`n", (Get-Content "./_Packaging/OpenCover.cobertura"))
    $got | Should -Be $expected.Replace("`r", "").Replace("\", [System.IO.Path]::DirectorySeparatorChar)

    $header = $x.Declaration.ToString() + "`n"
    ($header + $x.ToString()).Replace("`r", "") | Should -Be $expected.Replace("`r", "").Replace("\", [System.IO.Path]::DirectorySeparatorChar)
  }

  It "Converts NCover Data" {
    $lines = (Get-Content "./Tests/Sample1WithNCover.xml") | % { $_.Replace('excluded="true"', 'excluded="false"')}
    $lines | Set-Content "./_Packaging/NCover.cob.xml"
    $x = ConvertTo-Cobertura -InputFile "./_Packaging/NCover.cob.xml" -OutputFile "./_Packaging/NCover.cobertura"
    $coverage = $x.Descendants("coverage")
    $v = $coverage.Attribute("version").Value
    $t = $coverage.Attribute("timestamp").Value

    $expected = @"
<?xml version="1.0" encoding="utf-8" standalone="no"?>
<!DOCTYPE coverage SYSTEM "http://cobertura.sourceforge.net/xml/coverage-04.dtd">
<coverage line-rate="0.7" branch-rate="1" lines-covered="7" lines-valid="10" branches-covered="0" branches-valid="0" complexity="1" version="$v" timestamp="$t">
  <sources>
    <source>Sample1</source>
  </sources>
  <packages>
    <package name="Sample1.exe" line-rate="0.7" branch-rate="1" complexity="1">
      <classes>
        <class name="TouchTest.Program" filename="Sample1/Program.cs" line-rate="0.7" branch-rate="1" complexity="1">
          <methods>
            <method name="TouchTest.Program.Main" signature="" line-rate="0.7" branch-rate="1" complexity="1">
              <lines>
                <line number="11" hits="1" branch="false" />
                <line number="12" hits="1" branch="false" />
                <line number="13" hits="1" branch="false" />
                <line number="14" hits="1" branch="false" />
                <line number="15" hits="1" branch="false" />
                <line number="16" hits="1" branch="false" />
                <line number="18" hits="0" branch="false" />
                <line number="19" hits="0" branch="false" />
                <line number="20" hits="0" branch="false" />
                <line number="21" hits="1" branch="false" />
              </lines>
            </method>
          </methods>
          <lines>
            <line number="11" hits="1" branch="false" />
            <line number="12" hits="1" branch="false" />
            <line number="13" hits="1" branch="false" />
            <line number="14" hits="1" branch="false" />
            <line number="15" hits="1" branch="false" />
            <line number="16" hits="1" branch="false" />
            <line number="18" hits="0" branch="false" />
            <line number="19" hits="0" branch="false" />
            <line number="20" hits="0" branch="false" />
            <line number="21" hits="1" branch="false" />
          </lines>
        </class>
      </classes>
    </package>
  </packages>
</coverage>
"@
    $got = [String]::Join("`n", (Get-Content "./_Packaging/NCover.cobertura"))
    $got | Should -Be $expected.Replace("`r", "")

    $header = $x.Declaration.ToString() + "`n"
    ($header + $x.ToString()).Replace("`r", "") | Should -Be $expected.Replace("`r", "")
  }

  It "Converts With the pipeline" {
    $lines = (Get-Content "./Tests/Sample1WithNCover.xml") | % { $_.Replace('excluded="true"', 'excluded="false"')}
    $x = [xml] ($lines) | ConvertTo-Cobertura
    $coverage = $x.Descendants("coverage")
    $v = $coverage.Attribute("version").Value
    $t = $coverage.Attribute("timestamp").Value

    $expected = @"
<?xml version="1.0" encoding="utf-8" standalone="no"?>
<!DOCTYPE coverage SYSTEM "http://cobertura.sourceforge.net/xml/coverage-04.dtd">
<coverage line-rate="0.7" branch-rate="1" lines-covered="7" lines-valid="10" branches-covered="0" branches-valid="0" complexity="1" version="$v" timestamp="$t">
  <sources>
    <source>Sample1</source>
  </sources>
  <packages>
    <package name="Sample1.exe" line-rate="0.7" branch-rate="1" complexity="1">
      <classes>
        <class name="TouchTest.Program" filename="Sample1/Program.cs" line-rate="0.7" branch-rate="1" complexity="1">
          <methods>
            <method name="TouchTest.Program.Main" signature="" line-rate="0.7" branch-rate="1" complexity="1">
              <lines>
                <line number="11" hits="1" branch="false" />
                <line number="12" hits="1" branch="false" />
                <line number="13" hits="1" branch="false" />
                <line number="14" hits="1" branch="false" />
                <line number="15" hits="1" branch="false" />
                <line number="16" hits="1" branch="false" />
                <line number="18" hits="0" branch="false" />
                <line number="19" hits="0" branch="false" />
                <line number="20" hits="0" branch="false" />
                <line number="21" hits="1" branch="false" />
              </lines>
            </method>
          </methods>
          <lines>
            <line number="11" hits="1" branch="false" />
            <line number="12" hits="1" branch="false" />
            <line number="13" hits="1" branch="false" />
            <line number="14" hits="1" branch="false" />
            <line number="15" hits="1" branch="false" />
            <line number="16" hits="1" branch="false" />
            <line number="18" hits="0" branch="false" />
            <line number="19" hits="0" branch="false" />
            <line number="20" hits="0" branch="false" />
            <line number="21" hits="1" branch="false" />
          </lines>
        </class>
      </classes>
    </package>
  </packages>
</coverage>
"@
    $header = $x.Declaration.ToString() + "`n"
    ($header + $x.ToString()).Replace("`r", "") | Should -Be $expected.Replace("`r", "")
  }

  It "Converts Real NCover Data" {
    $ev = ""
    ConvertTo-Cobertura -InputFile "./Tests/GenuineNCover158.Xml" -OutputFile "./_Packaging/NCover158.cobertura" -ErrorVariable ev
    $ev | Should -BeFalse
  }
}

Describe "ConvertTo-NCover" {
  It "converts" {
      $xml = ConvertTo-NCover -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/HandRolledMonoNCover.xml"
      $xml | Should -BeOfType "System.Xml.XmlDocument"

      $sw = new-object System.IO.StringWriter @()
      $settings = new-object System.Xml.XmlWriterSettings @()
      $settings.Indent = $true
      $settings.IndentChars = "  "
      $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
      $xml.WriteTo($xw)
      $xw.Close()
      $written = [System.IO.File]::ReadAllText("./_Packaging/HandRolledMonoNCover.xml")
      $result = [xml](Get-Content "./_Packaging/HandRolledMonoNCover.xml")
      $time = $result.coverage.startTime

      $expected = @"
<?xml version="1.0" encoding="utf-8"?>
<coverage profilerVersion="OpenCover" driverVersion="OpenCover" startTime="$time" measureTime="$time" xmlns:msxsl="urn:schemas-microsoft-com:xslt" xmlns:user="urn:my-scripts">
  <module moduleId="6A-33-AA-93-82-ED-22-9D-F8-68-2C-39-5B-93-9F-74-01-76-00-9F" name="Sample1.exe" assembly="Sample1" assemblyIdentity="Sample1">
    <method excluded="false" instrumented="true" name=".ctor" class="TouchTest.Program" fullname="System.Void TouchTest.Program::.ctor()" />
    <method excluded="false" instrumented="true" name="Main" class="TouchTest.Program" fullname="System.Void TouchTest.Program::Main(System.String[])">
      <seqpnt visitcount="1" line="11" column="9" endline="11" endcolumn="10" offset="0" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="12" column="32" endline="12" endcolumn="33" offset="1" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="13" column="13" endline="13" endcolumn="14" offset="7" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="13" column="21" endline="13" endcolumn="22" offset="9" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="14" column="13" endline="14" endcolumn="14" offset="24" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="17" endline="15" endcolumn="18" offset="25" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="72" endline="15" endcolumn="73" offset="36" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="25" endline="15" endcolumn="26" offset="46" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="16" column="13" endline="16" endcolumn="14" offset="51" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="18" column="13" endline="18" endcolumn="14" offset="57" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="19" column="17" endline="19" endcolumn="18" offset="58" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="19" column="25" endline="19" endcolumn="26" offset="63" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="20" column="13" endline="20" endcolumn="14" offset="68" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="21" column="9" endline="21" endcolumn="10" offset="69" excluded="false" document="altcover/Sample1/Program.cs" />
    </method>
  </module>
</coverage>
"@
    $sw.ToString().Replace("`r", "") | Should -Be $expected.Replace("`r", "")
    $sw.ToString().Replace("`r", "") | Should -Be $written.Replace("`r", "")
  }

  It "converts with the pipeline" {
    $xml = [xml](Get-Content "./Tests/HandRolledMonoCoverage.xml") | ConvertTo-NCover
    $xml | Should -BeOfType "System.Xml.XmlDocument"

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $time = $xml.coverage.startTime

    $expected = @"
<?xml version="1.0" encoding="utf-8"?>
<coverage profilerVersion="OpenCover" driverVersion="OpenCover" startTime="$time" measureTime="$time" xmlns:msxsl="urn:schemas-microsoft-com:xslt" xmlns:user="urn:my-scripts">
  <module moduleId="6A-33-AA-93-82-ED-22-9D-F8-68-2C-39-5B-93-9F-74-01-76-00-9F" name="Sample1.exe" assembly="Sample1" assemblyIdentity="Sample1">
    <method excluded="false" instrumented="true" name=".ctor" class="TouchTest.Program" fullname="System.Void TouchTest.Program::.ctor()" />
    <method excluded="false" instrumented="true" name="Main" class="TouchTest.Program" fullname="System.Void TouchTest.Program::Main(System.String[])">
      <seqpnt visitcount="1" line="11" column="9" endline="11" endcolumn="10" offset="0" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="12" column="32" endline="12" endcolumn="33" offset="1" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="13" column="13" endline="13" endcolumn="14" offset="7" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="13" column="21" endline="13" endcolumn="22" offset="9" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="14" column="13" endline="14" endcolumn="14" offset="24" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="17" endline="15" endcolumn="18" offset="25" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="72" endline="15" endcolumn="73" offset="36" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="25" endline="15" endcolumn="26" offset="46" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="16" column="13" endline="16" endcolumn="14" offset="51" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="18" column="13" endline="18" endcolumn="14" offset="57" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="19" column="17" endline="19" endcolumn="18" offset="58" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="19" column="25" endline="19" endcolumn="26" offset="63" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="20" column="13" endline="20" endcolumn="14" offset="68" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="21" column="9" endline="21" endcolumn="10" offset="69" excluded="false" document="altcover/Sample1/Program.cs" />
    </method>
  </module>
</coverage>
"@
    $sw.ToString().Replace("`r", "") | Should -Be $expected.Replace("`r", "")
  }
}

Describe "ConvertTo-BarChart" {
  It "converts NCover" {
    $xml = ConvertTo-BarChart -InputFile "./Tests/GenuineNCover158.Xml" -OutputFile "./_Packaging/GenuineNCover158Chart.html"
    $xml | Should -BeOfType "System.Xml.XmlDocument"

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/GenuineNCover158Chart.html")
    $expected = [System.IO.File]::ReadAllText("./Tests/GenuineNCover158Chart.html")

    $result = $sw.ToString().Replace("`r", "").Replace("html >", "html>") 
    $result | Should -Be $expected.Replace("`r", "")
    $result | Should -Be $written.Replace("`r", "")
  }

  It "converts NCover through the pipeline" {
    $xml = [xml](Get-Content "./Tests/GenuineNCover158.Xml" ) | ConvertTo-BarChart
    $xml | Should -BeOfType "System.Xml.XmlDocument"

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $expected = [System.IO.File]::ReadAllText("./Tests/GenuineNCover158Chart.html")

    # swap out unique identifiers
    $result = $sw.ToString().Replace("`r", "").Replace("html >", "html>")
    $result = $result -replace "href\=`"\#[A-Z0-9]{7}`"","href=`"xxx`"" 
    $result = $result -replace "name\=`"\#[A-Z0-9]{7}`"","name=`"xxx`"" 
    $result = $result -replace "id\=`"[A-Z0-9]+class","id=`"xxxclass" 
    $result = $result -replace "id\=`"[A-Z0-9]+`"","id=`"xxx`"" 
    $result = $result -replace "toggle\([A-Z0-9]+class","toggle(xxxclass" 
    $result = $result -replace "toggle\([A-Z0-9]+\)","toggle(xxx)" 

    $expected = $expected -replace "href\=`"\#[A-Z0-9]{7}`"","href=`"xxx`"" 
    $expected = $expected -replace "name\=`"\#[A-Z0-9]{7}`"","name=`"xxx`"" 
    $expected = $expected -replace "id\=`"[A-Z0-9]+class","id=`"xxxclass" 
    $expected = $expected -replace "id\=`"[A-Z0-9]+`"","id=`"xxx`"" 
    $expected = $expected -replace "toggle\([A-Z0-9]+class","toggle(xxxclass" 
    $expected = $expected -replace "toggle\([A-Z0-9]+\)","toggle(xxx)" 

    $result | Should -Be $expected.Replace("`r", "")
  }

  It "converts OpenCover" {
    $xml = ConvertTo-BarChart -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/HandRolledMonoCoverage.html"
    $xml | Should -BeOfType "System.Xml.XmlDocument"

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/HandRolledMonoCoverage.html")
    $expected = [System.IO.File]::ReadAllText("./Tests/HandRolledMonoCoverage.html")

    $result = $sw.ToString().Replace("`r", "").Replace("html >", "html>") 
    $result | Should -Be $expected.Replace("`r", "")
    $result | Should -Be $written.Replace("`r", "")
  }
}

Describe "ConvertFrom-NCover" {
  It "converts" {
    $assemblies = @()
    $assemblies += "./_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1/Sample4.dll"
    $xml = ConvertFrom-NCover -InputFile "./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml" -Assembly $Assemblies -OutputFile "./_Packaging/AltCoverFSharpTypes.xml"
    $xml | Should -BeOfType "System.Xml.XmlDocument"

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/AltCoverFSharpTypes.xml")
    $expected = [System.IO.File]::ReadAllText("./Tests/AltCoverFSharpTypes.n3.xml")

    $xml = ConvertTo-XDocument $xml
    $hash = $xml.Descendants("Module").Attribute("hash").Value
    $time = $xml.Descendants("ModuleTime").Value
    $file = $xml.Descendants("File") | Select-Object -First 1
    $fullpath = [System.io.path]::GetDirectoryName($file.Attribute("fullPath").Value)

    $expected = $expected.Replace("09-23-DC-B3-65-CE-96-5D-B4-56-2A-3A-0D-5A-1B-09-3E-38-2B-22", $hash)
    $expected = $expected.Replace("2018-06-13T15:08:24.8840000Z", $time)
    $expected = $expected.Replace("Sample4|Program.fs", (Join-Path $fullpath "Program.fs"))
    $expected = $expected.Replace("Sample4|Tests.fs", (Join-Path $fullpath "Tests.fs"))

    $result = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8") 
    $result | Should -Be $expected.Replace("`r", "")
    $result | Should -Be $written.Replace("`r", "")
  }

  It "converts from the pipeline" {
    $assemblies = @()
    $assemblies += "./_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1/Sample4.dll"
    $xml = [xml](Get-Content  "./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml") | ConvertFrom-NCover -Assembly $Assemblies
    $xml | Should -BeOfType "System.Xml.XmlDocument"

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()

    $expected = [System.IO.File]::ReadAllText("./Tests/AltCoverFSharpTypes.n3.xml")

    $xml = ConvertTo-XDocument $xml
    $hash = $xml.Descendants("Module").Attribute("hash").Value
    $time = $xml.Descendants("ModuleTime").Value
    $file = $xml.Descendants("File") | Select-Object -First 1
    $fullpath = [System.io.path]::GetDirectoryName($file.Attribute("fullPath").Value)

    $expected = $expected.Replace("09-23-DC-B3-65-CE-96-5D-B4-56-2A-3A-0D-5A-1B-09-3E-38-2B-22", $hash)
    $expected = $expected.Replace("2018-06-13T15:08:24.8840000Z", $time)
    $expected = $expected.Replace("Sample4|Program.fs", (Join-Path $fullpath "Program.fs"))
    $expected = $expected.Replace("Sample4|Tests.fs", (Join-Path $fullpath "Tests.fs"))

    $result = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8") 
    $result | Should -Be $expected.Replace("`r", "") ###.Substring(0,11680)
  }
}

Describe "Compress-Branching" {
  It "Removes interior branches" {
    $xml = Compress-Branching -WithinSequencePoint -InputFile "./Tests/Compressible.xml" -OutputFile "./_Packaging/CompressInterior.xml"
	$xml | Should -BeOfType "System.Xml.XmlDocument"

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/CompressInterior.xml")
    $expected = [System.IO.File]::ReadAllText("./Tests/CompressInterior.xml").Replace("`r", "")

    $result = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8")  
    $result | Should -Be $expected
    $written.Replace("`r", "") | Should -Be  $expected
  }
  It "Unifies equivalent branches" {
    $xml = Compress-Branching -SameSpan -InputFile "./Tests/Compressible.xml" -OutputFile "./_Packaging/SameSpan.xml"
	$xml | Should -BeOfType "System.Xml.XmlDocument"

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/SameSpan.xml")
    $expected = [System.IO.File]::ReadAllText("./Tests/SameSpan.xml").Replace("`r", "")

    $result = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8")  
    $result | Should -Be $expected
    $written.Replace("`r", "") | Should -Be  $expected
  }
  It "DoesBoth" {
    $xml = [xml](Get-Content  "./Tests/Compressible.xml") | Compress-Branching -SameSpan -WithinSequencePoint -OutputFile "./_Packaging/CompressBoth.xml"
	$xml | Should -BeOfType "System.Xml.XmlDocument"

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/CompressBoth.xml")
    $expected = ([System.IO.File]::ReadAllText("./Tests/CompressBoth.xml")).Replace("`r", "")

    $result = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8") 
    $result | Should -Be $expected
    $written.Replace("`r", "") | Should -Be  $expected
  }
  It "DoesAtLeastOne" {
    $fail = $false
    try {
	  $xml = Compress-Branching -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/CompressBoth.xml"  
	  $fail = $true
	}
	catch {
	  $_.Exception.Message | Should -BeLikeExactly 'Parameter set cannot be resolved using the specified named parameters.*'
	}

	$fail | Should -BeFalse
  }
}

Describe "MergeCoverage" {
  It "Absorbs Unsupported Xml Files" {
    $xml = dir -recurse "./Tests/*.cobertura" | Merge-Coverage
	$xml | Should -BeOfType "System.Xml.XmlDocument"
	$xml.OuterXml | Should -BeFalse
  }

  It "Selects Single OpenCover Files" {
    $files = ("./Tests/HandRolledMonoCoverage.xml", "./Tests/Sample1WithNCover.xml","./Tests/OpenCover.cobertura")
    $xml = $files | Merge-Coverage
	$xml | Should -BeOfType "System.Xml.XmlDocument"
	$expected = [xml](Get-Content "./Tests/HandRolledMonoCoverage.xml")
	$xml.OuterXml | Should -BeExactly $expected.OuterXml.Replace(' standalone="yes"', '')
  }

  It "Selects Single NCoverCover Files" {
    $files = ("./Tests/HandRolledMonoCoverage.xml", "./Tests/Sample1WithNCover.xml","./Tests/OpenCover.cobertura")
    $xml = $files | Merge-Coverage -AsNCover -OutputFile "./_Packaging/Sample1WithNCoverOnly.xml"
	$xml | Should -BeOfType "System.Xml.XmlDocument"
	$expected = [xml](Get-Content "./Tests/Sample1WithNCover.xml")
	$xml.OuterXml | Should -BeExactly $expected.OuterXml
	$result = [xml](Get-Content "./_Packaging/Sample1WithNCoverOnly.xml")
	$result.OuterXml | Should -BeExactly $expected.OuterXml
  }

  It "Combines Top Level Summaries" {
    $files = ("./Tests/HandRolledMonoCoverage.xml", "./Tests/Sample4.Prepare.xml")
    $xml = $files | Merge-Coverage -OutputFile "./_Packaging/OpenCoverCombination-1.xml"
	$xml | Should -BeOfType "System.Xml.XmlDocument"
	$summary = ($xml.SelectNodes("//Summary") | Select-Object -First 1).OuterXml
	$summary | Should -BeExactly '<Summary numSequencePoints="36" visitedSequencePoints="0" numBranchPoints="17" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="11" minCyclomaticComplexity="1" visitedClasses="0" numClasses="7" visitedMethods="0" numMethods="11" minCrapScore="0" maxCrapScore="0" />'
  }

}