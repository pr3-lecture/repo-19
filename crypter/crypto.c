#include "crypto.h"
#include "stdio.h"
#include "memory.h"

typedef enum { false, true } bool;

//mode: 0 = encrypt; 1 = decrypt
int checkForIllegalChars(const char* input, int mode){

	char* allowedCharacters;

	if (mode == 0) {
		allowedCharacters = MESSAGE_CHARACTERS;
	} else {
		allowedCharacters = CYPHER_CHARACTERS;
	}

	int checkIfCorrect;

	for (int i = 0; i < strlen(input); i++) {
		checkIfCorrect = 0;

		for (int j = 0; j < strlen(allowedCharacters); j++) {
			if(input[i] == allowedCharacters[j]){
				checkIfCorrect = 1;
			}
		}

		if(checkIfCorrect == 0){
			return 0;
		}
	}
	return 1;
}

int crypt(KEY key, const char* input, char* output){

	int keyLength = strlen(key.chars);

	for (int i = 0; i < strlen(input); i++) {
		char newInput = input[i] - 'A' + 1;
		char newKey = key.chars[i % keyLength] - 'A' + 1;
		char newOutput = newInput ^ newKey;
		output[i] = newOutput + 'A' - 1;
	}
	output[strlen(input)] = '\0';

	return 0;
}

int encrypt(KEY key, const char* input, char* output){

	if(strlen(key.chars) < 2){
		fprintf(stderr, "%s\n", "KEY TOO SHORT");
		return E_KEY_TOO_SHORT;
	}

	if(checkForIllegalChars(key.chars, 1) == 0){
		fprintf(stderr, "%s\n", "ILLEGAL KEY");
		return E_KEY_ILLEGAL_CHAR;
	}

	if (checkForIllegalChars(input, 1) == 0) {
		fprintf(stderr, "%s\n", "ILLEGAL MESSAGE");
		return E_MESSAGE_ILLEGAL_CHAR;
	}

	crypt(key, input, output);

	return 0;
}

int decrypt(KEY key, const char* cypherText, char* output){

	if(strlen(key.chars) < 2){
		fprintf(stderr, "%s\n", "KEY TOO SHORT");
		return E_KEY_TOO_SHORT;
	}

	if(checkForIllegalChars(key.chars, 1) == 0){
		fprintf(stderr, "%s\n", "ILLEGAL KEY");
		return E_KEY_ILLEGAL_CHAR;
	}

	if (checkForIllegalChars(cypherText, 0) == 0) {
		fprintf(stderr, "%s\n", "ILLEGAL CYPHER");
		return E_CYPHER_ILLEGAL_CHAR;
	}

	crypt(key, cypherText, output);

	return 0;
}
