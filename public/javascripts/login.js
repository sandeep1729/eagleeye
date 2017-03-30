function LoginController($scope, $http) {
  
  $scope.feedback = '';
    
  $scope.postLoginForm = function(loginUrl, successUrl) {
    var data = {
      username : $scope.username,
      password : $scope.password
    };
    $scope.successUrl = successUrl;
    $http.post(loginUrl, data).success(function(data, status, headers, config) {
      if (data.valid) {
        document.location.href = successUrl;
      } else {
        $scope.feedback = "Invalid username / password. Try again.";
      }
    }).error(function(data, status, headers, config) {
      $scope.feedback = 'error: ' + data + ", " + status;
    });
  };
}