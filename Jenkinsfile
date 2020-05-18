@Library('klarna-shared-library@0.8.0')

def bitbucket = new klarna.bitbucket.BitBucket(this, 'jenkins-stash')
def email = new klarna.email.EmailNotification(this)
def git = new klarna.git.Git(this)

node('erlang-otp-r22') {
  timeout(time: 10, unit: 'MINUTES') {
    email.notifyAfter(to: 'pgw.e@klarna.com') {
      stage('Checkout') {
        deleteDir()
        checkout scm

        bitbucket.notify(key: 'Checkout', state: 'SUCCESSFUL')
      }

      def VERSION = git.describe()

      currentBuild.displayName = "#${currentBuild.number} (${VERSION})"

      env.REBAR_PROFILE="jenkins"

      bitbucket.notifiedStage('Fetch Dependencies') {
        sshagent(['jenkins-ssh']) {
          sh 'make get-deps'
        }
      }

      bitbucket.notifiedStage('Unit Tests') {
        try {
          sh 'make eunit'
          sh 'make cover'
          sh 'make covertool'
          sh 'utils/clean-eunit-xml _build/jenkins+test/*.xml'
        } finally {
          junit(testResults: '_build/jenkins+test/TEST-*.xml')
          archiveArtifacts '_build/jenkins+test/cover/'
          cobertura(coberturaReportFile: '_build/jenkins+test/covertool/*.xml')
        }
      }

      bitbucket.notifiedStage('Code Analysis') {
        sh 'make lint'
        sh 'make xref'
        sh 'make dialyzer'
      }
    }
  }
}
