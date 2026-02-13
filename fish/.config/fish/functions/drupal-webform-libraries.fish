function drupal-webform-libraries
    ddev drush webform:libraries:composer && ddev drush webform:libraries:download && ddev drush webform:composer:update -y && ddev composer update --lock
end
