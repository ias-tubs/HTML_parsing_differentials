CREATE DATABASE `mxssy` /*!40100 DEFAULT CHARACTER SET utf8mb4 */;
CREATE USER 'mxssy'@'localhost' IDENTIFIED BY 'allyourmutationsarebelongtous';
GRANT CREATE, ALTER, SELECT, INSERT, UPDATE, DELETE, DROP, REFERENCES, CREATE VIEW, SHOW VIEW ON mxssy.* TO 'mxssy'@'localhost';
FLUSH PRIVILEGES;


CREATE TABLE `browsers` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id_UNIQUE` (`id`),
  UNIQUE KEY `name_UNIQUE` (`name`)
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=utf8mb4;

CREATE TABLE `evaluations` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `gen_id` int(11) NOT NULL,
  `browser_id` int(11) NOT NULL,
  `sanitizer_id` int(11) NOT NULL,
  `timestamp` datetime NOT NULL DEFAULT current_timestamp(),
  `error_message` text DEFAULT NULL,
  `executed` int(11) NOT NULL,
  `sanitized` text DEFAULT NULL,
  `result` text DEFAULT NULL,
  `serialized` text DEFAULT NULL,
  `status` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `browser_Id` (`browser_id`),
  KEY `sanitizer_id` (`sanitizer_id`),
  KEY `fk_evaluations_1_idx` (`gen_id`),
  KEY `gen_id` (`gen_id`),
  CONSTRAINT `fk_browser` FOREIGN KEY (`browser_id`) REFERENCES `browsers` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_generation` FOREIGN KEY (`gen_id`) REFERENCES `generations` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_sanitizer` FOREIGN KEY (`sanitizer_id`) REFERENCES `sanitizers` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB AUTO_INCREMENT=257193 DEFAULT CHARSET=utf8mb4;

CREATE TABLE `generations` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `hash` bigint(20) NOT NULL,
  `payload` text NOT NULL,
  `pretty` text NOT NULL,
  `json` text NOT NULL,
  `timestamp` datetime NOT NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`id`),
  UNIQUE KEY `hash_UNIQUE` (`hash`)
) ENGINE=InnoDB AUTO_INCREMENT=1048568 DEFAULT CHARSET=utf8mb4;

CREATE TABLE `sanitizers` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id_UNIQUE` (`id`),
  UNIQUE KEY `name_UNIQUE` (`name`)
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=utf8mb4;


CREATE
VIEW `differeing_executions` AS
    SELECT
        `ec`.`gen_id` AS `gen_id`,
        `ec`.`executed` AS `chrome_exec`,
        `ew`.`executed` AS `webkit_executed`,
        `ef`.`executed` AS `firefox_executed`,
        `ec`.`executed` + `ew`.`executed` + `ef`.`executed` AS `total_executed`,
        `g`.`payload` AS `payload`,
        `g`.`pretty` AS `pretty`,
        `g`.`json` AS `json`,
        `ec`.`sanitized` AS `sanitized`,
        `ec`.`result` AS `chrome_result`,
        `ec`.`serialized` AS `chrome_serialized`,
        `ew`.`result` AS `wk_result`,
        `ew`.`serialized` AS `wk_serialized`,
        `ef`.`result` AS `ff_result`,
        `ef`.`serialized` AS `ff_serialized`
    FROM
        (((`evaluations` `ec`
        JOIN `evaluations` `ew` ON (`ec`.`gen_id` = `ew`.`gen_id`
            AND `ec`.`sanitizer_id` = `ew`.`sanitizer_id`))
        JOIN `evaluations` `ef` ON (`ec`.`gen_id` = `ef`.`gen_id`
            AND `ec`.`sanitizer_id` = `ef`.`sanitizer_id`))
        JOIN `generations` `g` ON (`ec`.`gen_id` = `g`.`id`))
    WHERE
        `ec`.`browser_id` = 1
            AND `ew`.`browser_id` = 2
            AND `ef`.`browser_id` = 3
            AND `ec`.`sanitizer_id` = 1
            AND `ew`.`sanitizer_id` = 1
            AND `ef`.`sanitizer_id` = 1
            AND `ec`.`executed` + `ew`.`executed` + `ef`.`executed` > 0
            AND `ec`.`executed` + `ew`.`executed` + `ef`.`executed` < 3;
