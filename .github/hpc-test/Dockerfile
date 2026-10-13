# syntax=docker/dockerfile:1
#
# A single-node SLURM cluster reachable over SSH — the CI stand-in for an institute's login node
# (docs/todo/HPC_PLAN.md → Decision 10). Used by .github/workflows/verify-hpc.yml; never shipped.
#
# P1 needs sshd + `sbatch --version`; P2 submits, cancels and times out real jobs, and installs the
# Python env with pixi (curl fetches pixi, as on a real site).
#
#   docker build -t cecelia-hpc-test .github/hpc-test
#   docker run -d --privileged -p 2222:22 -e AUTHORIZED_KEY="$(cat key.pub)" cecelia-hpc-test
FROM ubuntu:24.04

RUN apt-get update \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      openssh-server slurm-wlm munge ca-certificates curl \
 && rm -rf /var/lib/apt/lists/* \
 && useradd -m -s /bin/bash cecelia \
 && mkdir -p /run/sshd /var/spool/slurmctld /var/spool/slurmd /var/log/slurm \
 && chown slurm:slurm /var/spool/slurmctld /var/log/slurm

# Key-only, no root: the same posture as a real login node.
RUN printf '%s\n' 'PasswordAuthentication no' 'KbdInteractiveAuthentication no' 'PermitRootLogin no' \
      > /etc/ssh/sshd_config.d/cecelia.conf

COPY <<'EOF' /usr/local/bin/start-hpc-test
#!/bin/sh
set -e
# The authorised key arrives at run time, so no key is baked into the image.
install -d -m 700 -o cecelia -g cecelia /home/cecelia/.ssh
printf '%s\n' "$AUTHORIZED_KEY" > /home/cecelia/.ssh/authorized_keys
chown cecelia:cecelia /home/cecelia/.ssh/authorized_keys
chmod 600 /home/cecelia/.ssh/authorized_keys
ssh-keygen -A

# slurm.conf names the node by hostname, which is only known now.
host=$(hostname -s)
cat > /etc/slurm/slurm.conf <<CONF
ClusterName=cecelia-ci
SlurmctldHost=$host
SlurmUser=slurm
AuthType=auth/munge
ProctrackType=proctrack/linuxproc
TaskPlugin=task/none
JobAcctGatherType=jobacct_gather/none
SchedulerType=sched/backfill
SelectType=select/cons_tres
ReturnToService=2
StateSaveLocation=/var/spool/slurmctld
SlurmdSpoolDir=/var/spool/slurmd
SlurmctldLogFile=/var/log/slurm/slurmctld.log
SlurmdLogFile=/var/log/slurm/slurmd.log
$(slurmd -C | head -n 1) State=UNKNOWN
PartitionName=debug Nodes=$host Default=YES MaxTime=INFINITE State=UP
CONF
# A container has no systemd/dbus to delegate a cgroup scope, which slurmd's cgroup/v2 plugin asks
# for at start-up; `IgnoreSystemd` makes it use the cgroup tree directly (needs a writable
# /sys/fs/cgroup, hence `docker run --privileged` in verify-hpc.yml).
printf 'CgroupPlugin=cgroup/v2\nIgnoreSystemd=yes\n' > /etc/slurm/cgroup.conf
# slurmd puts its stepd scope under system.slice, which a fresh cgroup namespace does not have.
mkdir -p /sys/fs/cgroup/system.slice || true
# Best effort here: a daemon that fails shows up in `sinfo` in the workflow's diagnostics step, and
# as the P2 live job tests failing.
service munge start || true
slurmctld || true
slurmd || true

exec /usr/sbin/sshd -D -e
EOF
RUN chmod +x /usr/local/bin/start-hpc-test

EXPOSE 22
CMD ["/usr/local/bin/start-hpc-test"]
