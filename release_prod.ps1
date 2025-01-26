param($message)

# create a function to get the last commit message
function Get-LastCommitMessage {
    $lastCommit = Invoke-Expression 'git log -1 --pretty=format:%s'
    return $lastCommit
}

function Speak-Message ($message) {
    $voice = New-Object -ComObject Sapi.spvoice
    
    # get voices, use one where Id contains ZIRA
    $voices = $voice.GetVoices()
    $voice.Voice = $voices | Where-Object { $_.Id.Contains("ZIRA") }
    $voice.rate = 0
    $voice.speak($message)
}

function Exit-With-Error ($message) {
    write-host $message
    Speak-Message $message
    exit 1
}

# check if there are any git changes, and if there are, report them and exit
$gitStatus = git status --porcelain
if ($null -ne $gitStatus) {
    
    # store message as multiline string
    $message = "
There are uncommitted changes in git, please make sure everything is committed before doing a release.
Git status:
$gitStatus
"
    Exit-With-Error $message
}

if ([System.String]::IsNullOrEmpty($message))
{
    $lastCommit = Get-LastCommitMessage
    $warningMessage = "Message is missing, would you like to use the last commit message: $lastCommit"
    write-host $warningMessage
    Speak-Message $warningMessage
    
    $response = Read-Host "y/n"
    if ($response -eq "y")
    {
        $message = $lastCommit
    }
    else
    {
        Exit-With-Error "Please provide a message"
    }
}

# short hand to use the last commit message, don't ask why 'y' is used
if ($message -eq "y")
{
    $message = Get-LastCommitMessage
}

# ensure that $messsage has "'" escaped
$message = $message -replace "'", "''"

Invoke-Expression "dotnet build -c Release"
$exitCode = $LASTEXITCODE
if ($exitCode -ne 0) {
    Exit-With-Error "Build failed"
}

Invoke-Expression 'git push'

$v = Invoke-Expression 'git describe --tags --abbrev=0'

write-host $v

$version = new-object System.Version($v.Substring(1))

$newVersion = new-object System.Version($version.Major, $version.Minor, ($version.Build + 1))

$cmd = "git tag -a v$($newVersion) -m '$message'"
Invoke-Expression $cmd
Invoke-Expression "git push --tags"

$cmd = "git checkout prod"
Invoke-Expression $cmd

$cmd = "git merge main -m '$($newVersion): $message'"
Invoke-Expression $cmd

$cmd = "git push"
Invoke-Expression $cmd

$cmd = "git checkout main"
Invoke-Expression $cmd