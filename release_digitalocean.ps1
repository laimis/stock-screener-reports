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

# make sure garbage collection is not in progress
$garbageCollection = $true
while ($garbageCollection) {
    $collectionJson = invoke-expression 'doctl registry garbage-collection get-active --output json'
    
    $gcCollections = ConvertFrom-Json ([System.String]::Join("", $collectionJson))

    if ($gcCollections.Count -eq 0 -or $gcCollections.errors -ne $null)
    {
        $garbageCollection = $false
    }
    else
    {
        $statusDesc = $gcCollections[0].status
        write-host "Garbage collection is in progress: $statusDesc ..."
        Start-Sleep -Seconds 30
    }
}

Invoke-Expression 'git push'

$v = Invoke-Expression 'git describe --tags --abbrev=0'

write-host $v

$version = new-object System.Version($v.Substring(1))

$newVersion = new-object System.Version($version.Major, $version.Minor, ($version.Build + 1))

$cmd = "git tag -a v$($newVersion) -m '$message'"
Invoke-Expression $cmd
Invoke-Expression "git push --tags"

# clean up garbage
$manifestsJson = invoke-expression 'doctl registry repository list-manifests web --output json'

$manifests = ConvertFrom-Json ([System.String]::Join("", $manifestsJson))

foreach($manifest in $manifests)
{
    if ($manifest.tags.Length -ne 0)
    {
        continue
    }

    write-host "Deleting manifest $($manifest.digest)"
    Invoke-Expression "doctl registry repository delete-manifest web $($manifest.digest) -f"
}