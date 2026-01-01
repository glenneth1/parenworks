#!/bin/bash
# Deploy ParenWorks to tfs.is
# Run from local machine: ./deploy/deploy.sh

set -e

SERVER="tfs.is"
REMOTE_DIR="/opt/parenworks"

echo "=== Deploying ParenWorks to $SERVER ==="

# 1. Create directory on server
echo "Creating remote directory..."
ssh $SERVER "mkdir -p $REMOTE_DIR"

# 2. Sync files (excluding git, deploy scripts)
echo "Syncing files..."
rsync -avz --delete \
    --exclude '.git' \
    --exclude 'deploy/' \
    --exclude '*.fasl' \
    --exclude '*~' \
    ./ $SERVER:$REMOTE_DIR/

# 3. Set up Radiance environment on server
echo "Setting up Radiance environment..."
ssh $SERVER "mkdir -p ~/.config/radiance/parenworks/radiance-core ~/.config/radiance/parenworks/i-hunchentoot"

# Create i-hunchentoot config with port 3000
ssh $SERVER "cat > ~/.config/radiance/parenworks/i-hunchentoot/i-hunchentoot.conf.lisp << 'EOF'
; meta (:version 1.0 :package \"I-HUNCHENTOOT\")
[hash-table equal (:enabled (:default))
 (:default (:address \"0.0.0.0\" :port 3000))]
EOF"

# 4. Install Shirakumo dist if needed
echo "Ensuring Shirakumo dist is installed..."
ssh $SERVER "sbcl --load ~/quicklisp/setup.lisp --eval '(handler-case (ql-dist:find-dist \"shirakumo\") (error () (ql-dist:install-dist \"http://dist.shirakumo.org/shirakumo.txt\" :prompt nil)))' --eval '(quit)'"

# 5. Install systemd service
echo "Installing systemd service..."
ssh $SERVER "cat > /etc/systemd/system/parenworks.service << 'EOF'
[Unit]
Description=ParenWorks Systems Website (Radiance/Common Lisp)
After=network.target

[Service]
Type=simple
User=root
WorkingDirectory=/opt/parenworks
ExecStart=/usr/bin/sbcl --load /root/quicklisp/setup.lisp --load /opt/parenworks/start.lisp --eval \"(loop (sleep 3600))\"
Restart=on-failure
RestartSec=10

[Install]
WantedBy=multi-user.target
EOF"

ssh $SERVER "systemctl daemon-reload"

# 6. Update Caddy config
echo "Updating Caddy configuration..."
ssh $SERVER "grep -q 'parenworks.systems' /etc/caddy/Caddyfile || cat >> /etc/caddy/Caddyfile << 'EOF'

parenworks.systems {
    reverse_proxy localhost:3000
}

www.parenworks.systems {
    redir https://parenworks.systems{uri}
}
EOF"

# 7. Reload Caddy
echo "Reloading Caddy..."
ssh $SERVER "systemctl reload caddy"

# 8. Start/restart ParenWorks service
echo "Starting ParenWorks service..."
ssh $SERVER "systemctl enable parenworks"
ssh $SERVER "systemctl restart parenworks"

# 9. Check status
echo ""
echo "=== Deployment complete ==="
ssh $SERVER "systemctl status parenworks --no-pager | head -15"

echo ""
echo "Site should be available at https://parenworks.systems once DNS is configured"
echo ""
echo "DNS Records needed at Namecheap:"
echo "  Type: A    Host: @    Value: $(ssh $SERVER 'curl -s ifconfig.me')"
echo "  Type: A    Host: www  Value: $(ssh $SERVER 'curl -s ifconfig.me')"
