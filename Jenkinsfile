def json

node ('Docker') {
    stage ('Checkout') {
        checkout scm
    }
    withDockerRegistry(credentialsId: '6d50b250-e0a3-4240-91de-b11a1b206597') {
        stage ('Build JSONServer Container') {
            json=docker.build('dyalog/jsonserver', '--no-cache .')
        }
        stage ('Publish JSONServer Container') {
            json.push();
        }
    }
    stage ('Cleanup') {
        sh 'docker image prune -f'
    }
}
