namespace FsKame

open Microsoft.Maui.ApplicationModel

module Audio =
    let haveRecordPermission () =
        task {
            let! permission =
                MainThread.InvokeOnMainThreadAsync<PermissionStatus>(fun () ->
                    Permissions.RequestAsync<Permissions.Microphone>())

            return permission = PermissionStatus.Granted
        }
        |> Async.AwaitTask
