# This file lists users who are allowed to log into the server via SSH and can
# redeploy the application.
{
  trh = {
    sshKeys = [
      "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIBqc66g7ma8C7SBhm9l3xHGcJgxavYY7HTsrSIaVj8h4AAAACHNzaDptaXJh trh@mira"
    ];
  };

  fabrizio = {
    sshKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKAq2z7PICdQq1Qi7jIF8qsq+W+BPJY5QhmUjRwHSv7P fabrizio@aurelius"
    ];
  };

  pacchettibotti = {
    sshKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGm5hQeEQS85APheDeSRyggo0Igdq6CbTfq1f8eQG45t pacchettibotti@purescript.org"
    ];
  };
}
